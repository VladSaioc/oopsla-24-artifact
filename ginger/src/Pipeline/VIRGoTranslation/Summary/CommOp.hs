module Pipeline.VIRGoTranslation.Summary.CommOp where

import Control.Monad (unless)
import Data.Map qualified as M
import Data.Maybe

import Backend.Ast
import VIRGo.Ast qualified as I
import VIRGo.Utilities
import Pipeline.VIRGoTranslation.Summary.Chan
import Pipeline.VIRGoTranslation.Utilities
import Utilities.Collection

-- | Mappings from concurrent operation type to a set of
-- program points marking channel operations with that direction.
type 𝒪s = CommOpType ↦ [𝒪]

-- | Communication operation summary.
data 𝒪 = 𝒪
  { -- | Process of channel operation
    oP :: P,
    -- | Concurrency primitive name
    o𝐶 :: 𝐶,
    -- | Concurrent operation
    oDir :: CommOpType,
    -- | Program point
    o𝑛 :: 𝑁
  } deriving (Eq, Read)

instance Show 𝒪 where
  show 𝒪 {oP = p, o𝐶, oDir, o𝑛 = n} =
    -- p: c{!,?} <n>
    unwords [show p ++ ":", o𝐶 ++ show oDir, "<" ++ show n ++ ">"]

-- | Aggregates all non-loop channel operations across
-- all processes of the program, including operation
-- direction, program point, and channel name.
noloopPsChanInsns :: I.𝑆 -> P ↦ (𝐶 ↦ 𝒪s)
noloopPsChanInsns = programToCollection noloopPChanInsns

{- | Aggregates all non-loop channel operations, including operation
direction, program point, and channel name.
Depends on: 𝑛, p, p', S

Rules:

> [SKIP]:    𝑛, p, p' ⊢ skip -> []

> [RETURN]:  𝑛, p, p' ⊢ return -> []

> [FOR]:     𝑛, p, p' ⊢ for (i : e .. e) { s } -> []

> [SEND]:    𝑛, p, p' ⊢ c! -> p ↦ [c ↦ [! ↦ {𝑛}]]

> [RECV]:    𝑛, p, p' ⊢ c? -> p ↦ [c ↦ [? ↦ {𝑛}]]

> [SEQ]:     ⟨𝑛, 𝑆₁; 𝑆₂⟩ -> M₁ ⊔ M₂
>            ↳ ⟨𝑛, 𝑆₁⟩ -> M₁
>            ↳ ⟨𝑛', 𝑆₂⟩ -> M₂

> [IF]:
-}
noloopPChanInsns :: 𝛬 -> I.𝑆 -> P ↦ (𝐶 ↦ 𝒪s)
noloopPChanInsns 𝛬 { 𝑛, p } = \case
   -- Atomic operations are added to the list of triples.
   I.Atomic o -> case opType o of
      -- Only focus on channel operations.
      CommO d ->
        let c = primName o
            o' = 𝒪 {oP = p, o𝐶 = c, oDir = d, o𝑛 = 𝑛}
         in M.empty ⇒ (p, o' +> M.empty)
      _ -> M.empty
   -- All other statements return an empty map, or are traversed
   -- recursively in inductive cases.
   _ -> M.empty

-- | Inserts a channel operation into a channel operation map.
-- Given, a triple (c, d, n) where c is a channel name, d
-- is the direction of an operation, and n is the program point
-- of the operation, and a map of channel operations M,
-- the result is:
--
-- > M[c ↦ M(c)[d ↦ M(c)(d) ∪ {n}]]
--
-- If M(c) is undefined (and similarly M(c)(d)), they get initialized
-- to the corresponding zero value for the appropriate type.
(+>) :: 𝒪 -> (𝐶 ↦ 𝒪s) -> (𝐶 ↦ 𝒪s)
ch@(𝒪 {o𝐶 = c, oDir = d}) +> chops =
  let ops = fromMaybe M.empty (M.lookup c chops)
      dops = fromMaybe [] (M.lookup d ops)
   in chops ⊔ (c, d, ch : dops)

-- | A pattern for identifying asynchronous send encodings. Corresponds to:
--
-- > { c := c' + 1; p := 𝑛; }
pattern AsyncSend :: String -> String -> String -> Int -> Stmt
pattern AsyncSend c c' p n =
  Block
    [ -- c := c + 1
      Assign c (EVar c' :+ ECon (CNum 1)),
      -- p := 𝑛
      Assign p (ECon (CNum n))
      ]

-- | A pattern for identifying asynchronous receive encodings. Corresponds to:
--
-- > { c := c' - 1; p := 𝑛; }
pattern AsyncRecv :: String -> String -> String -> Int -> Stmt
pattern AsyncRecv c c' p n =
  Block
    [ -- c := c - 1
      Assign c (EVar c' :- ECon (CNum 1)),
      -- p := 𝑛
      Assign p (ECon (CNum n))
      ]

-- | A pattern for identifying synchronous send encodings. Corresponds to:
--
-- > { c := 1; p := 𝑛; }
pattern SyncSend :: String -> String -> Int -> Stmt
pattern SyncSend c p n =
  Block
    [ -- c := 1
      Assign c (ECon (CNum 1)),
      -- p := 𝑛
      Assign p (ECon (CNum n))
      ]

-- | A pattern for identifying synchronous receive encodings. Corresponds to:
--
-- > { c := -1; p := 𝑛; }
pattern SyncRecv :: String -> String -> Int -> Stmt
pattern SyncRecv c p n =
  Block
    [ -- c := -1
      Assign c (ECon (CNum (-1))),
      -- p := 𝑛
      Assign p (ECon (CNum n))
      ]

-- | Checks whether a back-end statement encodes a channel send or receive
-- operation, and returns the name of the channel if that is the case.
-- The result is wrapped in "Left" for channel sends, and "Right" for channel
-- receives. Channel operations have the following pattern (members between
-- angle brackets correspond to code generated for send on the left side,
-- and receive on the right side):
--
-- > if 0 < 𝜅(c) {
-- >    if c ⟨< 𝜅(c) | > 0⟩ {
-- >       c := c ⟨+ | -⟩ 1;
-- >       p := n;
-- >    }
-- > } else {
-- >    if c == ⟨0 | 1⟩ {
-- >       c := ⟨1 | -1⟩;
-- >       p := n';
-- >    }
-- > }
-- where n' = ⟨n + 1 | n⟩
backendChannelOp :: Stmt -> Maybe (Either 𝐶 𝐶)
backendChannelOp =
  let -- Return a result inside a pair
      result c x = return (c, x)
      -- Results that conform to the "channel send" pattern pair
      -- relevant results with the "Left" constructor.
      send = result Left
      -- Results that conform to the "channel receive" pattern pair
      -- relevant results with the "Right" constructor.
      recv = result Right
      -- Check that the communication case is a block with a single if-statement
      -- without an else case. This is satisfied by both synchronous and
      -- asynchronous communication:
      -- { if guard { body } }
      -- Yields the guard and the body.
      commCase = \case
        Block [If guard body Nothing] -> return (guard, body)
        _ -> Nothing
   in \case
        -- The channel operation must be modelled as a statement that
        -- changes the semantics of the operation based on whether the channel
        -- is buffered or unbuffered.
        --
        -- This requires an if statement of the form:
        -- if 0 < 𝜅(c) { asynchronous case } else { synchronous case }
        If capGuard async (Just sync) -> do
          -- Check that the capacity guard is of the form: 0 < 𝜅(c)
          k <- case capGuard of
            ECon (CNum 0) :< k -> return k
            _ -> Nothing

          -- Get the asynchronous case guard and body.
          (aGuard, aBody) <- commCase async
          -- Check that the asynchronous operation guard is valid.
          -- The produced constructor "d" informs the next steps
          -- what is the direction of the channel operation
          -- ("Left" for send, "Right" for receive).
          --
          -- Produces c: The name of the channel used in the guard.
          (d, c) <- case aGuard of
            -- For send, the guard checks that the channel is not full.
            -- It also checks that the capacity expression is consistent
            -- between the capacity and the operation guards:
            -- c < 𝜅(c)
            EVar c :< k' -> do
              unless (k == k') Nothing
              send c
            -- For receive, the guard checks that the channel is not empty:
            -- c > 0
            EVar c :> ECon (CNum 0) -> recv c
            _ -> Nothing
          -- Check that the statement conforms to asynchronous operation encoded.
          -- The constructor "d" produced at the previous step ensures
          -- that the check produces consistent results.
          --
          -- Yields the following:
          -- - p1: the process variable being modified
          -- - n1: the program point of the next operation
          -- - c1: the name of the channel variable on the LHS of the assignment
          -- - c2: the name of the channel variable on the RHS of the assignment
          (d1, (p1, n1, c1, c2)) <- case d aBody of
            -- For send, the semantics is encoded as incrementing the
            -- channel buffer length, and progressing to the next operation.
            Left (AsyncSend c1 c2 p1 n1) -> send (p1, n1, c1, c2)
            -- For receive, the semantics is encoded as decrementing the
            -- channel buffer length, and progressing to the next operation.
            Right (AsyncRecv c1 c2 p1 n1) -> recv (p1, n1, c1, c2)
            _ -> Nothing

          -- Get the synchronous case guard and body.
          (sGuard, sBody) <- commCase sync
          -- Check that the synchronous operation guard is valid.
          -- The produced constructor "d2" informs the next steps
          -- what is the direction of the channel operation
          -- ("Left" for send, "Right" for receive).
          --
          -- Produces c3: The name of the channel used in the guard.
          (d2, c3) <- case d1 sGuard of
            -- For send, the guard checks that the channel is ready to send
            -- (channel value is encoded as 0):
            -- c == 0
            Left (EVar c3 :== ECon (CNum 0)) -> send c3
            -- For receive, the guard checks that the channel is ready to
            -- synchronize (channel value is encoded as 1):
            -- c == 1
            Right (EVar c3 :== ECon (CNum 1)) -> recv c3
            _ -> Nothing
          -- Check that the statement conforms to asynchronous operation encoded.
          -- The constructor "d" produced at the previous step ensures
          -- that the check produces consistent results.
          --
          -- Yields the following:
          -- - p2: the process variable being modified
          -- - n2: the program point of the next operation
          -- - c4: the name of the channel variable on the LHS of the assignment
          (dfinal, (p2, n2, c4)) <- case d2 sBody of
            -- For send, the semantics is encoded as incrementing the
            -- channel buffer length, and progressing to the next operation.
            -- The return program point is offset by 1, because the immediate next point
            -- is occupied by the synchronization 'rendezvous' operation.
            Left (SyncSend c4 p2 n2) -> send (p2, n2 + 1, c4)
            Right (SyncRecv c4 p2 n2) -> recv (p2, n2, c4)
            _ -> Nothing
          -- Check that the process name is consistent between the
          -- synchronous and asynchronous cases.
          unless (p1 == p2) Nothing
          -- Check that the switch to the next operation is consistent
          -- between the two cases.
          unless (n1 == n2) Nothing
          -- Check that the channel names used throughout the operation
          -- are consistent.
          unless (all (== c) [c1, c2, c3, c4]) Nothing
          -- Return the channel name, wrapped in "Left" if the operation
          -- is a send, or "Right" if the operation is a receive.
          return (dfinal c)
        -- The statement does not conform to any channel operation pattern.
        _ -> Nothing

{- | Convert back-end instruction point to channel operation summary.
Depends on: p, 𝜙, 𝑛

! Summary does not include path conditions.

Produces:

> 𝒪 {p, c, d, 𝑛}, where cd = 𝜙(𝑛)
-}
insnToChOpSummary :: P -> 𝑁 -> Stmt -> Maybe 𝒪
insnToChOpSummary p 𝑛 s = do
  op <- backendChannelOp s
  let (c, d) = either (,S) (,R) op
  return
    𝒪
      { oP = p,
        o𝐶 = c,
        oDir = d,
        o𝑛 = 𝑛
      }

{- | Aggregate all channel operation points from all processes, indexed.
Produces a map of channel operation summaries indexed by the channel name.
Depends on: p, 𝜙

Produces:

> [ c ↦ 𝒪 {p, c, d, 𝑛} | (𝑛, cd) ∈ 𝜙 . d ∈ {!, ?} ]
-}
chanOpsMap :: 𝛯 -> P ↦ (𝐶 ↦ [𝒪])
chanOpsMap = M.mapWithKey processChanOpsMap

{- | Aggregate all channel operation points from a given process and its program points.
Produces a map of channel operation summaries indexed by the channel name.
Depends on: p, 𝜙

Produces:

> [ c ↦ 𝒪 {p, c, d, 𝑛} | (𝑛, cd) ∈ 𝜙 . d ∈ {!, ?} ]
-}
processChanOpsMap :: P -> 𝛷 -> 𝐶 ↦ [𝒪]
processChanOpsMap p =
  let addChanOp 𝑛 i cops =
        case insnToChOpSummary p 𝑛 i of
          Just o@(𝒪 {o𝐶 = c}) -> M.insertWith (++) c [o] cops
          Nothing -> cops
   in M.foldrWithKey addChanOp M.empty

{- | Aggregate all channel operation points from a given map of program points.
Produces a list of channel operation summaries, including the channel name,
process id, operation direction, program point.
Depends on: p, 𝜙

Produces:

> { 𝒪 {p, c, d, 𝑛} | (𝑛, cd) ∈ 𝜙. d ∈ {!, ?} }
-}
processChanOps :: P -> 𝛷 -> [𝒪]
processChanOps p = catMaybes . M.elems . M.mapWithKey (insnToChOpSummary p)

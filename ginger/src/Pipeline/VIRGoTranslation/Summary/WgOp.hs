module Pipeline.VIRGoTranslation.Summary.WgOp where

import Control.Monad (unless)
import Data.Map qualified as M
import Data.Set
import Data.Maybe

import Backend.Ast
import Backend.Utilities
import VIRGo.Ast qualified as I
import VIRGo.Utilities
import Pipeline.VIRGoTranslation.Utilities
import Utilities.Collection
import Pipeline.VIRGoTranslation.Exps

-- | Short-hand for sets of WaitGroup names.
type 𝑊 = Set 𝑋

-- | Mappings from concurrent operation type to a set of
-- program points marking channel operations with that direction.
type 𝒲s = WgOpType ↦ [𝒲]

-- | WaitGroup operation summary.
data 𝒲 = 𝒲
  { -- | Process of channel operation
    wP :: P,
    -- | Concurrency primitive name
    w𝐶 :: 𝑋,
    -- | Concurrent operation
    wDir :: WgOpType,
    -- | Expression for Add operation
    wE :: Exp,
    -- | Program point
    w𝑛 :: 𝑁
  } deriving (Eq, Read)

instance Show 𝒲 where
  show 𝒲 {wP = p, w𝐶, wDir, w𝑛 = n} =
    -- p: c{!,?} <n>
    unwords [show p ++ ":", w𝐶 ++ show wDir, "<" ++ show n ++ ">"]

-- | Aggregates all non-loop channel operations across
-- all processes of the program, including operation
-- direction, program point, and channel name.
noloopPsWgInsns :: I.𝑆 -> P ↦ (String ↦ 𝒲s)
noloopPsWgInsns = programToCollection noloopWgInsns

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
noloopWgInsns :: 𝛬 -> I.𝑆 -> P ↦ (𝑋 ↦ 𝒲s)
noloopWgInsns 𝛬 { 𝑛, p } = \case
   -- Atomic operations are added to the list of triples.
   I.Atomic o ->
      case opType o of
        WgO d ->
          let c = primName o
              e = case o of
                  I.Add _ e' ->  parseExp e'
                  _ -> (0 #)
              w' = 𝒲 {wP = p, w𝐶 = c, wDir = d, w𝑛 = 𝑛, wE = e}
          in M.empty ⇒ (p, w' +> M.empty)
        _ -> M.empty
   -- All other statements return an empty map, or are traversed
   -- recursively in inductive cases.
   _ -> M.empty

-- | Inserts a WaitGroup operation summary into a WaitGroup operation map.
-- Given, a triple (w, d, n) where w is a WaitGroup name, d
-- is the direction of an operation, and n is the program point
-- of the operation, and a map of WaitGroup operations M,
-- the result is:
--
-- > M[c ↦ M(c)[d ↦ M(c)(d) ∪ {n}]]
--
-- If M(c) is undefined (and similarly M(c)(d)), they get initialized
-- to the corresponding zero value for the appropriate type.
(+>) :: 𝒲 -> (𝑋 ↦ 𝒲s) -> (𝑋 ↦ 𝒲s)
ch +> chops =
  let 𝒲 {w𝐶 = c, wDir = d} = ch
      ops = fromMaybe M.empty (M.lookup c chops)
      dops = fromMaybe [] (M.lookup d ops)
      dops' = ch : dops
      ops' = M.insert d dops' ops
   in M.insert c ops' chops

-- | A pattern for identifying WaitGroup addition. Corresponds to:
--
-- > { w' := w'' + e'; p := 𝑛; }
pattern WaitGroupAdd :: String -> String -> Exp -> String -> Int -> Stmt
pattern WaitGroupAdd w w' e p n =
  Block
    [ -- c := c + 1
      Assign w (EVar w' :+ e),
      -- p := 𝑛
      Assign p (ECon (CNum n))
      ]

-- | A pattern for identifying WaitGroup addition guards. Coressponds to:
--
-- > w + e >= 0
pattern WgAddGuard :: String -> Exp -> Exp
pattern WgAddGuard w e = ((EVar w) :+ e) :>= (ECon (CNum 0))

-- | A pattern for identifying WaitGroup Waits. Corresponds to:
pattern WaitGroupWait :: String -> String -> Int -> Stmt
pattern WaitGroupWait w p n =
  If (EVar w :== ECon (CNum 0)) (Block [Assign p (ECon (CNum n))]) Nothing


-- | Checks whether a back-end statement encodes a WaitGroup Add or Wait
-- operation, and returns the name of the WaitGroup if that is the case.
-- The result is wrapped in "Left" for Add, and "Right" for Wait.
-- WaitGroup have the following pattern (members between
-- angle brackets correspond to code generated for send on the left side,
-- and receive on the right side):
--
-- > if ⟨w + e >= 0 | w == 0⟩ {
-- >    ⟨w := w + e | {}⟩;
-- >    p := n;
-- > }
-- where n' = ⟨n + 1 | n⟩
backendWgOp :: Stmt -> Maybe (Either (String, Exp) String)
backendWgOp = \case
  -- Wait operations have a simple pattern. Simply get the name of the WaitGroup.
  WaitGroupWait w _ _ -> return $ Right w
  s -> do
    -- Get the case guard and body.
    (aGuard, body) <- case s of
        -- Check that the program point is a block with a single if-statement
        -- without an else case.
        -- Yields the guard and the body.
        If guard body Nothing -> return (guard, body)
        _ -> Nothing
    -- Check that the operation guard is valid.
    -- The produced constructor "d" informs the next steps
    -- what is the direction of the channel operation
    -- ("Left" for Add, "Right" for Waot).
    --
    -- Produces w: The name of the WaitGroup in the guard.
    -- Produces e: The expression to compare to (is the Add expression)
    (w, e) <- case aGuard of
      -- For Add, the guard checks that the counter is not negative after applying the Add expression:
      -- c + e >= 0
      WgAddGuard w e -> return (w, e)
      _ -> Nothing
    -- Check that the statement conforms to asynchronous operation encoded.
    -- The constructor "d" produced at the previous step ensures
    -- that the check produces consistent results.
    --
    -- Yields the following:
    -- - e1: The expression with which the counter is incremented
    -- - w1: the name of the WaitGroup variable on the LHS of the assignment
    -- - w2: the name of the WaitGroup variable on the RHS of the assignment
    (e1, w1, w2) <- case body of
      -- For Add, the semantics is encoded as incrementing the
      -- WaitGroup counter, and progressing to the next operation.
      WaitGroupAdd w1 w2 e1 _ _ -> return (e1, w1, w2)
      _ -> Nothing
    -- Check that the Addition expression is consistent between the
    -- guard and the increment expression.
    unless (e == e1) Nothing
    -- Check that the WaitGroup names used throughout the operation
    -- are consistent.
    unless (all (== w) [w1, w2]) Nothing
    -- Return the WaitGroup name and Add expression, wrapped in "Left".
    return $ Left (w, e)

{- | Convert back-end instruction point to a WaitGroup operation summary.
Depends on: p, 𝜙, 𝑛

Produces:

> 𝒲 {p, c, d, 𝑛}, where cd = 𝜙(𝑛)
-}
insnToWgOpSummary :: P -> 𝑁 -> Stmt -> Maybe 𝒲
insnToWgOpSummary p 𝑛 s = do
  op <- backendWgOp s
  let makeAdd (w, e) =
        𝒲
          { wP = p,
            w𝐶 = w,
            wDir = A,
            wE = e,
            w𝑛 = 𝑛
          }
  let makeWait w =
        𝒲
          { wP = p,
            w𝐶 = w,
            wDir = W,
            wE = (0 #),
            w𝑛 = 𝑛
          }
  return $ either makeAdd makeWait op

{- | Aggregate all channel operation points from all processes, indexed.
Produces a map of channel operation summaries indexed by the channel name.
Depends on: p, 𝜙

Produces:

> [ c ↦ 𝒲 {p, c, d, 𝑛} | (𝑛, cd) ∈ 𝜙 . d ∈ {!, ?} ]
-}
chanOpsMap :: 𝛯 -> P ↦ (𝑋 ↦ [𝒲])
chanOpsMap = M.mapWithKey processWgOpsMap

{- | Aggregate all channel operation points from a given process and its program points.
Produces a map of channel operation summaries indexed by the channel name.
Depends on: p, 𝜙

Produces:

> [ c ↦ 𝒲 {p, c, d, 𝑛} | (𝑛, cd) ∈ 𝜙 . d ∈ {Add(e), Wait} ]
-}
processWgOpsMap :: P -> 𝛷 -> String ↦ [𝒲]
processWgOpsMap p =
  let addOp 𝑛 i cops =
        case insnToWgOpSummary p 𝑛 i of
          Just o@(𝒲 {w𝐶 = c}) -> M.insertWith (++) c [o] cops
          Nothing -> cops
   in M.foldrWithKey addOp M.empty

{- | Aggregate all channel operation points from a given map of program points.
Produces a list of channel operation summaries, including the channel name,
process id, operation direction, program point.
Depends on: p, 𝜙

Produces:

> { 𝒲 {p, c, d, 𝑛} | (𝑛, cd) ∈ 𝜙. d ∈ {!, ?} }
-}
processWgOps :: P -> 𝛷 -> [𝒲]
processWgOps p = catMaybes . M.elems . M.mapWithKey (insnToWgOpSummary p)

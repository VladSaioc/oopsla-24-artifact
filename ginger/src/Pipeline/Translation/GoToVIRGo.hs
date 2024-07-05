module Pipeline.Translation.GoToVIRGo (getIR) where

import Control.Monad (foldM_)
import Data.Map qualified as M
import Data.Maybe qualified as Mb

import Go.Ast qualified as P
import VIRGo.Ast
import Utilities.Err
import Utilities.General
import Utilities.Position
import Utilities.PrettyPrint (PrettyPrint (prettyPrint))
import Utilities.TransformationCtx

-- | Go-to-IR translation context.
data Ctxt a b = Ctxt
  { -- | Remaining syntax to translate.
    syntax :: a,
    -- | Next available loop counter.
    loopcounter :: Int,
    -- | Next available select decision point counter.
    casecounter :: Int,
    -- | Environment from Go variables to VIRGo expressions.
    varenv :: M.Map String 𝐸,
    -- | Environment from channel names in the scope to their declaration names.
    chenv :: M.Map String String,
    -- | Environment from WaitGroup names in the scope to their declaration names.
    wgenv :: M.Map String String,
    -- | Translation object so far.
    curr :: b
  }
  deriving (Eq, Ord, Read)

instance TransformCtx Ctxt where
  source = syntax
  updateSource ctx a = ctx {syntax = a}
  object = curr
  updateObject ctx a = ctx {curr = a}

-- | Convert whole Go program to VIRGo.
getIR :: P.Prog -> Err 𝑆
getIR (P.Prog ss) =
  let ρ =
        Ctxt
          { -- Begin translation with entry process statements.
            syntax = ss,
            -- Next loop counter is 0
            loopcounter = 0,
            -- Next case counter is 0
            casecounter = 0,
            -- All initial environments are empty
            varenv = M.empty,
            chenv = M.empty,
            wgenv = M.empty,
            -- First object statement is a skip
            curr = Skip
          }
   in do
        -- Translate all Go statements and get an exit translation context.
        ρ' <- translateStatements ρ
        -- Obtain VIRGo program.
        return $ curr ρ'

-- | Translate Go statements to VIRGo, ensuring that only VIRGo supported
-- features are used.
translateStatements :: Ctxt [Pos P.Stmt] 𝑆 -> Err (Ctxt () 𝑆)
translateStatements ρ = case syntax ρ of
  [] -> done ρ
  Pos p s : ss -> case s of
    -- Pass skip statements.
    P.Skip -> translateStatements (ss >: ρ)
    -- Add a return statement and drop the continuation as unreachable.
    P.Return -> translateStatements ([] >: ρ <: Seq (curr ρ) Return)
    -- Do not add a break statement, but drop the continuation as unreachable.
    P.Break -> translateStatements ([] >: ρ)
    -- Pass continue statements.
    P.Continue -> translateStatements (ss >: ρ)
    -- Value declaration.
    P.Decl x e -> do
      -- Get variable environment.
      let venv = varenv ρ
      -- Traslate right-hand side expression.
      e' <- translateExp venv e
      -- Bind the variable next to the translated expression.
      let ρ₁ = ρ {varenv = M.insert x e' venv}
      -- Continue translation with the continuation.
      translateStatements (ss >: ρ₁)
    P.If e ss1 ss2 -> do
      -- Translate guard expression.
      e' <- translateExp (varenv ρ) e
      -- Translate true branch block
      ρ₁ <- translateStatements (ss1 >: ρ <: Skip)
      -- Translate false branch block
      ρ₂ <- translateStatements (ss2 >: ρ₁ <: Skip)
      -- The translation object becomes:
      -- obj(ρ); if e' { obj(ρ₁) } else { obj(ρ₂) }
      let ρ₃ = ρ₂ <: Seq (curr ρ) (If e' (curr ρ₁) (curr ρ₂))
      -- Continue translation of continuation.
      translateStatements (ss >: ρ₃)
    -- Atomic communication operations outside loops are added
    -- as wrapped in the 'Atomic' constructor before they go to the VIRGo.
    P.Atomic op -> do
      -- Translate operation with current context.
      ρ' <- translateOp $ op >: ρ
      -- Wrap operation in the 'Atomic' constructor.
      let op' = Atomic $ curr ρ'
      -- Translation object becomes:
      -- obj(ρ); op'
      -- Translate continuation.
      translateStatements (ss >: ρ' <: Seq (curr ρ) op')
    -- Close operations are translated 1-to-1
    P.Close c -> do
      c' <- mlookup ("Invalid channel: value not found: " ++ c) c (chenv ρ)
      translateStatements (ss >: ρ <:  Seq (curr ρ) (Close c'))
    -- Translate WaitGroup declaration
    P.Wgdef w -> do
      -- Insert WaitGroup name into the WaitGroup name environment
      -- (bound to itself initially).
      let ρ₁ = ρ { wgenv = M.insert w w (wgenv ρ) }
      -- Translate continuation.
      translateStatements (ss >: ρ₁ <: Seq (curr ρ) (Def (Wg w)))
    P.Add e w -> do
      w' <- mlookup ("Invalid WaitGroup: value not found: " ++ show w) w (wgenv ρ)
      e' <- translateExp (varenv ρ) e
      translateStatements (ss >: ρ <: Seq (curr ρ) (Atomic $ Add w' e'))
    P.Wait w -> do
      w' <- mlookup ("Invalid WaitGroup: value not found: " ++ show w) w (wgenv ρ)
      translateStatements (ss >: ρ <: Seq (curr ρ) (Atomic $ Wait w'))
    -- Translate channel declaration.
    P.Chan c e -> do
      -- Translate capacity expression.
      e' <- translateExp (varenv ρ) e
      -- Insert channel into capacity environment.
      -- Insert channel name into the channel name environment
      -- (bound to itself initially).
      let ρ₁ =
            ρ
              { chenv = M.insert c c (chenv ρ)
              }
      -- Translate continuation.
      translateStatements (ss >: ρ₁ <: Seq (curr ρ) (Def (Chan c e')))
    -- Translate go statement
    P.Go ss' -> do
      -- Translate go statement body with a fresh context.
      ρ₁ <-
        translateStatements
          Ctxt
            { -- Syntax is the body of the go statement.
              syntax = ss',
              -- Case counter inherited from current context.
              casecounter = casecounter ρ,
              -- Loop counter inherited from current context.
              loopcounter = loopcounter ρ,
              -- Variable environment inherited from current context.
              varenv = varenv ρ,
              -- Channel name environment inherited from current context.
              chenv = chenv ρ,
              -- WaitGroup name environment inherited from current context.
              wgenv = wgenv ρ,
              -- Translation object is initially skip.
              curr = Skip
            }
      -- Propagate persistent information from the resulting translation context
      -- to the context of the continuation.
      -- Translate continuation.
      translateStatements (ss >: ρ₁ <: Seq (curr ρ) (Go (curr ρ₁)))
    P.For x e1 e2 diff ss' -> do
      -- Get variable environment.
      let venv = varenv ρ
      -- Translate the bound expressions and place them in a pair.
      (e1', e2') <- binaryCons (translateExp venv) (,) e1 e2
      -- Translate the body of the for loop.
      ρ' <- translateFor (ss' >: ρ <: [])
      -- Change the order of the bounds depending on whether the loop uses ++ or --.
      let for = case diff of
            -- Incrementing loops preserve the position of the bounds.
            P.Inc -> For (x ++ "'" ++ show (loopcounter ρ')) e1' e2' $ curr ρ'
            -- Decrementing loops flip the position of the bounds.
            P.Dec -> For (x ++ "'" ++ show (loopcounter ρ')) e2' e1' $ curr ρ'
      -- Translation object becomes:
      -- obj(ρ); for x e1' e2' { s' }
      -- Increment loop counter.
      let ρ'' = (ρ <: Seq (curr ρ) for) {loopcounter = loopcounter ρ' + 1}
      --  Translate continuation.
      translateStatements (ss >: ρ'')
    -- Flatten block statements.
    P.Block ss' -> translateStatements $ (ss' ++ ss) >: ρ
    P.Select cs Nothing -> do
      let -- Get select cases operating on named channels.
          -- Ensure only one such case exists.
          getChannelCase r cas@(Pos p' o, _) = do
            c <- r
            case o of
              -- Unknown channel operations are not an issue.
              P.Star -> return c
              -- Return send case, if found.
              P.Send _ -> maybe (return $ return cas) (const $ posErr p' "Multiple channel operations in 'select") c
              -- Return receive case, if found.
              P.Recv _ -> maybe (return $ return cas) (const $ posErr p' "Multiple channel operations in 'select") c
      -- Get channel case operation.
      c <- Prelude.foldl getChannelCase (return Nothing) cs
      -- Translate channel case operation and case arm.
      ρ' <- case c of
        -- If the channel operation was found.
        Just (Pos _ o, ss') -> do
          -- Translating the channel operation in the case arm.
          ρ₁ <- translateOp $ o >: ρ
          let o' = curr ρ₁
          -- Translate case body.
          ρ₂ <- translateStatements $ ss' >: ρ₁ <: Skip
          -- Translation object is a sequence between the channel guard
          -- and the translated case body.
          return $ ρ₂ <: Seq (Atomic o') (curr ρ₂)
        -- If all cases are on unknown channels, do not do anything.
        Nothing -> done (ρ <: Skip)
      -- Translate a single case arm.
      let translateSelect mρ (Pos _ o, s'') = do
            -- Get translation context so far.
            ρ₀ <- mρ
            case o of
              -- If the case is an operation on an unknown channel.
              P.Star -> do
                -- Translate case body.
                ρ₁ <- translateStatements $ s'' >: ρ₀ <: Skip
                -- Construct a symbolic guard for the select case statement.
                let guard = Var ("S'" ++ show (casecounter ρ₁))
                -- Construct an if statement simulating whether the select took
                -- the case arm. Put the case body under the then branch.
                -- Put the other translation object under the else branch.
                let select = If guard (curr ρ₁) (curr ρ₀)
                -- Increment case counter.
                let ρ₂ = ρ₁ {casecounter = casecounter ρ₁ + 1}
                -- Translation object becomes the new if statement.
                return $ ρ₂ <: select
              -- If the operation is a case on a known channel, do not anything,
              -- because we have already handled this.
              _ -> return ρ₀
      -- Fold all select cases, using the channel case arm (if present) as the
      -- starting point.
      ρ₂ <- Prelude.foldl translateSelect (return ρ') cs
      -- Translation object becomes:
      -- obj(ρ); obj(ρ₂)
      -- Where obj(ρ₂) is the translated select statement.
      let ρ₃ = ρ₂ <: Seq (curr ρ) (curr ρ₂)
      -- Translate continuation.
      translateStatements (ss >: ρ₃)
    _ -> posErr p ("Unexpected statement: " ++ show s)

-- Translation of the 'for' body converts it to an IR
-- sequence of channel operations. Conditions are assumed
-- to not have any communicating operations underneath them.
--
-- Rules:
--  [DONE]:       [] ===> ϵ
--  [SKIP]:       skip; s ===> s'
--                |- s ===> s'
--  [CONTINUE]:   continue; s ===> s'
--                |- s ===> s'
--  [SEND]:       c!; s ===> c!; s'
--                |- s ===> s'
--  [RECV]:       c?; s ===> c?; s'
--                |- s ===> s'
--  [ADD]:        w.Add(e); s ===> w.Add(e); s'
--                |- s ===> s'
--  [WAIT]:       w.Wait(); s ===> w.Wait(); s'
--                |- s ===> s'
--  [IF]:         if _ ; s ===> s'
--                |- s ===> s'
--  [SELECT]:     select _ ; s ===> s
--                |- s ===> s'
translateFor :: Ctxt [Pos P.Stmt] [Op] -> Err (Ctxt () [Op])
translateFor ρ = case syntax ρ of
  [] -> done $ ρ <: reverse (curr ρ)
  Pos p s : ss -> case s of
    P.Atomic op -> do
      ρ₁ <- translateOp $ op >: ρ
      let ρ₂ = ρ₁ <: (curr ρ₁ : curr ρ)
      translateFor $ ss >: ρ₂
    P.Add e w -> do
      e' <- translateExp (varenv ρ) e
      let ρ₁ = ρ <: (Add w e' : curr ρ)
      translateFor $ ss >: ρ₁
    P.Wait w -> do
      let ρ₁ = ρ <: (Wait w : curr ρ)
      translateFor $ ss >: ρ₁
    P.Skip -> translateFor $ ss >: ρ
    P.Continue -> translateFor $ ss >: ρ
    P.If _ s1 s2 -> do
      _ <- translateFor $ s1 >: ρ
      _ <- translateFor $ s2 >: ρ
      translateFor $ ss >: ρ
    P.Select cs def -> do
      foldM_ (\_ s1 -> translateFor (s1 >: ρ)) (() >: ρ) $ map snd cs
      foldM_ (\_ s1 -> translateFor (s1 >: ρ)) (() >: ρ) def
      translateFor $ ss >: ρ
    _ -> posErr p $ "Go-to-IR: Unexpected statement: " ++ prettyPrint 0 s

-- | Expression translation is a straightforward translation from Go expressions
-- to VIRGo translations.
--
-- > [VAR]:     σ ⊢ x ==> σ(x)
-- > [CONST]:   σ ⊢ c ==> c
-- >            |- c ∈ {true, false} ∪ ℤ
-- > [BINARY]:  σ ⊢ E₁ ⨁ E₂ ==> E₁' ⨁ E₂'
-- >            |- E₁ ==> E₁'
-- >            |- E₁ ==> E₂'
-- >            |- ⨁ ∈ { &&, ||, ==, !=, >=, >, <, <=, +, -, *, /}
-- > [NOT]:     σ ⊢ !E₁ ==> !E₁'
-- >            |- E₁ ==> E₁'
-- > [NEG]:     σ ⊢ -E₁ ==> 0 - E₁'
-- >            |- E₁ ==> E₁'
translateExp :: M.Map String 𝐸 -> P.Exp -> Err 𝐸
translateExp venv =
  let bin = binaryCons (translateExp venv)
   in \case
        P.CTrue -> return BTrue
        P.CFalse -> return BFalse
        P.And e1 e2 -> bin (:&) e1 e2
        P.Or e1 e2 -> bin (:|) e1 e2
        P.Not e -> unaryCons (translateExp venv) Not e
        -- -e ===> 0 - e
        P.Neg e -> do
          e' <- translateExp venv e
          return $ Const 0 :- e'
        P.Eq e1 e2 -> bin (:==) e1 e2
        P.Ne e1 e2 -> bin (:!=) e1 e2
        P.Le e1 e2 -> bin (:<=) e1 e2
        P.Lt e1 e2 -> bin (:<) e1 e2
        P.Ge e1 e2 -> bin (:>=) e1 e2
        P.Gt e1 e2 -> bin (:>) e1 e2
        P.CNum n -> return $ Const n
        P.Plus e1 e2 -> bin (:+) e1 e2
        P.Minus e1 e2 -> bin (:-) e1 e2
        P.Mult e1 e2 -> bin (:*) e1 e2
        P.Div e1 e2 -> bin (:/) e1 e2
        P.Var x -> return $ Mb.fromMaybe (Var x) $ M.lookup x venv

-- | Communication operation translation is straightfoward.
--
-- > [SEND]:      c! ==> c!
-- > [RECEICE]:   c? ==> c?
translateOp :: Ctxt P.CommOp a -> Err (Ctxt () Op)
translateOp ρ =
  let translate cons c = do
        c' <- mlookup ("Invalid channel: value not found: " ++ c) c (chenv ρ)
        done (ρ <: cons c')
   in case syntax ρ of
        P.Send c -> translate Send c
        P.Recv c -> translate Recv c
        s -> fail $ "Unexpected communication operation: " ++ show s

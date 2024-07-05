module Pipeline.Sanity.GoAllowed (allowed) where

import Control.Monad (foldM)
import Data.Maybe
import Data.Set qualified as S

import Go.Ast
import Utilities.Err
import Utilities.General
import Utilities.Position

type Ident = String

-- | Context for checking whether a Go program
-- conforms to the restrictions imposed by Ginger
type Ctxt a = ICtxt a ()
data ICtxt a b = Ctxt
  { -- Set of channel names
    chans :: S.Set String,
    -- Set of channel names
    wgs :: S.Set String,
    -- Set of loop variables
    loopVars :: S.Set String,
    -- Set of concurrency parameters
    commParams :: S.Set String,
    -- Set of mutable variables
    mutableVars :: S.Set String,
    -- Current loop depth
    loopDepth :: Int,
    -- Is true if the current statement is preceded by a continue
    continue :: Bool,
    -- Is true if the current statement is conditionally executed in a loop
    conditional :: Bool,
    -- Syntax of source language
    syntax :: a
  }
  deriving (Eq, Ord, Read, Show)


-- | A program is allowed if all of its processes are allowed,
-- and they follow the pattern:
--  1. Check for allowed declarations
--  2. Check for allowed goroutine spawns
--  3. Check for allowed statements
allowed :: Prog -> Err ()
allowed (Prog ss) =
  let newCtxt =
        Ctxt
          { chans = S.empty,
            wgs = S.empty,
            loopVars = S.empty,
            commParams = S.empty,
            mutableVars = S.empty,
            loopDepth = 0,
            continue = False,
            conditional = False,
            syntax = ss
          }
   in allowedDeclarations newCtxt

-- | Checks that all the declaration in a program are allowed.
allowedDeclarations :: Ctxt [Pos Stmt] -> Err ()
allowedDeclarations ρ = case syntax ρ of
  [] -> return ()
  Pos _ s : ss ->
    let ρ' = ρ {syntax = ss}
     in case s of
          Block ss' -> allowedDeclarations (ρ {syntax = ss' ++ ss })
          _ -> do
            _ <- allowedStmts ρ'
            return ()

-- | Check that all statements in a Go program consist strictly
-- of allowed features.
allowedStmts :: Ctxt [Pos Stmt] -> Err (Ctxt ())
allowedStmts ρ =
  let ok = return (ρ {syntax = ()})
   in case syntax ρ of
        [] -> ok
        Pos p s : ss ->
          let ρ' = ρ {syntax = ss}
              err = posErr p
              (!) prop msg = if prop then return () else err msg
           in case s of
                -- No declarations are allowed after the declaration phase
                -- is over.
                -- Non-channel declarations are allowed and earmarked
                -- as concurrency parameters as long as the RHS is allowed.
                Decl _ e -> do
                  cxs <- expVars e
                  -- Proceed to next statement and earmark any concurrency parameters
                  -- discovered in the RHS expression
                  allowedStmts $ ρ' {commParams = S.union (commParams ρ') cxs}
                -- Channel declarations are allowed and earmarked
                -- as long as the capacity expression is allowed.
                Chan x e -> do
                  cxs' <- expVars e
                  let ρ2 =
                        ρ'
                          { -- Earmark channel name
                            chans = S.insert x (chans ρ'),
                            -- Earmark capacity-related concurrency parameters
                            commParams = S.union (commParams ρ') cxs'
                          }
                  allowedStmts ρ2
                Wgdef x -> do
                  let ρ2 =
                        ρ'
                          { -- Earmark WaitGroup name
                            wgs = S.insert x (wgs ρ')
                          }
                  allowedStmts ρ2
                -- Close operations are not supported (yet), but allowed
                -- as long as verification succeeds without them, effectively
                -- proving that they are redundant.
                Close {} -> allowedStmts ρ'
                -- WaitGroup Add operations are always allowed at the top level.
                -- Inside loops, they are only allowed if they may not be skipped
                -- due to preceding 'continue' statements or loop body path conditions.
                Add e _ -> do
                  cxs' <- expVars e
                  let ρ₁ =
                        ρ'
                          { -- Earmark Add-related concurrency parameters
                            commParams = S.union (commParams ρ) cxs'
                          }
                  not (continue ρ) ! "WaitGroup.Add operation might be skipped due to continue."
                  (loopDepth ρ == 0 || not (conditional ρ)) ! "WaitGroup.Add operation is conditional in loop."
                  allowedStmts ρ₁
                -- WaitGroup Wait operations are always allowed at the top level.
                -- Inside loops, they are only allowed if they may not be skipped
                -- due to preceding 'continue' statements or loop body path conditions.
                Wait _ -> do
                  not (continue ρ) ! "Wait operation might be skipped due to continue."
                  (loopDepth ρ == 0 || not (conditional ρ)) ! "Wait operation is conditional in loop."
                  allowedStmts ρ'
                -- Irregular loops are not supported
                While {} -> err "Unexpected 'while' loop"
                -- Select statements with a single passive case are reduced to the branch
                Select [(Pos _ Star, ss')] Nothing -> do
                  ρ₁ <- allowedStmts (ρ { syntax = ss' })
                  allowedStmts (ρ₁ { syntax = ss })
                -- Select statements with a single active case are reduced to the branch
                -- preceded by the case operation.
                Select [(Pos p' o, ss')] Nothing -> do
                  ρ₁ <- allowedStmts (ρ { syntax = Pos p' (Atomic o) : ss' })
                  allowedStmts (ρ₁ { syntax = ss })
                -- Select statements with multiple cases.
                Select cs def -> do
                  -- Check whether all the select cases are on irrelevant channels.
                  let allStars = all ((==) Star . (@^) . fst) cs
                  -- Active 'select' statements inside loops are not allowed
                  (loopDepth ρ == 0 || allStars) ! "Found active 'select' inside loop"
                  -- Counts the number of active (non-*) cases in the select statement.
                  let activeCases = length $ filter ((/=) Star . (@^) . fst ) cs
                  -- 'select' statements outside loops must have a single active branch and no default case.
                  ((activeCases /= 1) || isNothing def) ! "Found 1 active 'select' case with default branch. Non-blocking active 'select' statements are not supported (yet)."
                  -- Passive 'select' statements may have a 'default' case.
                  (activeCases <= 1) ! "Found more than one active 'select' case"
                  -- Check that every case branch is allowed.
                  let checkOneCase (ρᵢ, hasCont) ss' = do
                        ρₖ <- allowedStmts ρᵢ {syntax = ss', conditional = loopDepth ρ /= 0, continue = continue ρ}
                        return (ρₖ, hasCont || continue ρₖ)
                  (ρ₁, hasCont) <- foldM checkOneCase (ρ { syntax = () }, continue ρ) $ map snd cs
                  -- Check that the default case branch is allowed
                  (ρ₂, hasCont') <- foldM checkOneCase (ρ₁, hasCont) def
                  -- Check that the continuation is allowed.
                  -- The 'conditional' flag is inherited from the parent context.
                  -- The 'continue' flag is a disjunction between the current 'continue' flag and
                  -- every 'continue' flag produced by the case branches.
                  allowedStmts (ρ₂ { syntax = ss, continue = hasCont', conditional = conditional ρ })
                -- Block statements are reduced to their contents.
                Block ss' -> do
                  ρ₁ <- allowedStmts (ρ {syntax = ss'})
                  allowedStmts (ρ₁ {syntax = ss})
                -- Goroutine spawns are not allowed after the spawning
                -- phase is over.
                Go ss' -> do
                  _ <- allowedDeclarations (ρ {syntax = ss'})
                  allowedStmts ρ'
                -- Atomic operations are always allowed at the top level.
                -- Inside loops, they are only allowed if they may not be skipped
                -- due to preceding 'continue' statements or loop body path conditions.
                Atomic _ -> do
                  not (continue ρ) ! "Communication operation might be skipped due to continue."
                  (loopDepth ρ == 0 || not (conditional ρ)) ! "Communication operation is conditional in loop."
                  allowedStmts ρ'
                -- If statements check both cases for legal operations.
                If _ s1 s2 -> do
                  -- Either branch is checked as conditional if inside a loop.
                  ρ₁ <- allowedStmts (ρ {syntax = s1, conditional = loopDepth ρ /= 0})
                  -- The 'else' branch inherits the continue flag from the parent context.
                  ρ₂ <- allowedStmts (ρ₁ {syntax = s2, conditional = loopDepth ρ /= 0, continue = continue ρ})
                  -- The continuation of the if statement joins the current 'continue' flag with
                  -- those produced by the case branches
                  allowedStmts (ρ₂ {syntax = ss, continue = continue ρ || continue ρ₁ || continue ρ₂})
                -- Return statements are only supported outside loops.
                Return -> do
                  (loopDepth ρ == 0) ! "Found 'return' in loop"
                  ok
                -- Break statements are only supported outside loops, in which case they are no-ops.
                Break -> do
                  (loopDepth ρ == 0) ! "Found 'break' in loop"
                  ok
                -- Continue statements switch the continue flag to true (if inside a loop)
                -- for the in-scope continuation of the continue statement.
                Continue -> allowedStmts (ρ' { continue = loopDepth ρ /= 0 })
                For x e1 e2 _ body -> do
                  -- Do not allow nested loops.
                  (loopDepth ρ == 0) ! "Found nested loops"
                  -- Extract all concurrency-influecing variables (variables used
                  -- in loop bound expressions, and the loop index variable).
                  xs' <- binaryCons expVars S.union e1 e2
                  let xs = S.insert x xs'
                  let ρ₂ =
                        ρ
                          { loopVars = S.union xs $ loopVars ρ,
                            loopDepth = 1,
                            syntax = body
                          }
                  -- Make sure that concurrency variables are not mutable
                  -- (except loop index variables in the context of their own loop).
                  S.disjoint xs (mutableVars ρ₂) ! "Assignment to loop-relevant variable."
                  ρ₃ <- allowedStmts ρ₂
                  allowedStmts (ρ₃ {syntax = ss, continue = continue ρ, loopDepth = loopDepth ρ})
                -- Assignments are only allowed if they do not affect concurrency variables.
                As x e -> do
                  let x' = S.singleton x
                  S.disjoint x' (loopVars ρ) ! "Assignment to loop-relevant variable."
                  S.disjoint x' (commParams ρ) ! "Assignment to concurrency parameter."
                  _ <- expVars e
                  let ρ₂ = ρ' {mutableVars = S.union x' $ mutableVars ρ}
                  allowedStmts ρ₂
                -- Skip statements are always allowed.
                Skip -> allowedStmts ρ'

-- | Collect all variables used in expressions.
expVars :: Exp -> Err (S.Set Ident)
expVars =
  let bin = binaryCons expVars S.union
   in \case
        CNum _ -> return S.empty
        CTrue -> return S.empty
        CFalse -> return S.empty
        And e1 e2 -> bin e1 e2
        Or e1 e2 -> bin e1 e2
        Eq e1 e2 -> bin e1 e2
        Ne e1 e2 -> bin e1 e2
        Le e1 e2 -> bin e1 e2
        Lt e1 e2 -> bin e1 e2
        Ge e1 e2 -> bin e1 e2
        Gt e1 e2 -> bin e1 e2
        Plus e1 e2 -> bin e1 e2
        Minus e1 e2 -> bin e1 e2
        Mult e1 e2 -> bin e1 e2
        Div e1 e2 -> bin e1 e2
        Neg e -> expVars e
        Not e -> expVars e
        Var x -> return $ S.singleton x

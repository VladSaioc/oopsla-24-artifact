module Pipeline.VIRGoTranslation.Encoding where

import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe qualified as Mb
import Data.Set qualified as S

import Backend.Ast
import Backend.Utilities
import Backend.Simplifier (eSimplify)
import VIRGo.Ast qualified as I
import VIRGo.Utilities
import Pipeline.VIRGoTranslation.Summary.Chan
import Pipeline.VIRGoTranslation.Summary.Summary
import Pipeline.VIRGoTranslation.Summary.WgOp
import Pipeline.VIRGoTranslation.Utilities
import Utilities.Collection

{- | An abstract representation of the encoding.
-}
data Encoding = Encoding
  { -- | Original VIRGo program
    prog :: I.𝑆,
    -- | Reachability conditions of program points in the encoding.
    conditions :: 𝛹,
    -- | Channel capacities
    capacities :: 𝛫,
    -- | WaitGroups
    waitgroups :: 𝑊,
    -- | Type environment for concurrency parameters
    typeenv :: 𝛤,
    -- | Type variables required by polymorphic concurrency parameters
    typevars :: [Type],
    -- | Process encodings
    processes :: 𝛯,
    -- | High-level syntax summaries
    summaries :: ℳ,
    -- | Expressions encoding projected number of communication operations.
    comprojection :: 𝐶 ↦ (CommOpType ↦ Exp),
    -- | Expression encoding projected number of WaitGroup operations.
    wgprojection :: 𝐶 ↦ (WgOpType ↦ Exp),
    -- | Closed channels
    closes :: S.Set 𝐶,
    -- | Per-process postcondition
    post :: Exp
  }

-- | Get 'balanced-flow' precondition from the encoding.
balancedFlowPre :: Encoding -> Exp
balancedFlowPre Encoding { conditions = 𝜓, summaries = ℳ { cs }, capacities = 𝜅, comprojection = comms, wgprojection = w } =
   let prc 𝒞 { c𝐶 = c, cP, c𝑛 = 𝑛} =
         let -- Get channel capacity expression.
            k = 𝜅 M.! c
            -- Get channel operations (defaults to 0 if none are present.)
            os = Mb.fromMaybe (M.fromList [(S, (0 #)), (R, (0 #))]) (M.lookup c comms)
            -- Get projected number of sends
            sends = os M.! S
            -- Get projected number of receives
            recvs = os M.! R
            -- Construct receive unblock precondition.
            -- Receives unblock if there are more sends.
            rcvsUnblock = recvs :<= sends
            -- Construct send unblock precondition.
            -- Sends unblock if there are more receive operations and
            -- capacity combined.
            sndsUnblock = sends :<= (recvs :+ k)
         in -- If the channel definition is reachable, apply the balanced flow heuristic
            𝜓 M.! cP M.! 𝑛 :==> (rcvsUnblock :&& sndsUnblock)
       prw os = (os M.! A) :== (0 #)
    in ((L.map prc cs ++ M.elems (M.map prw w)) ...⋀)

-- | Checks whether there are any channels without send operations.
-- If there are no send operations and partial deadlocks are considered guaranteed,
-- likely the channel escapes the fragment scope, or is used as a free variable in a thunk,
-- (i.e. we are dealing with a false positive).
noSendsFound :: Encoding -> Bool
noSendsFound Encoding { comprojection = p } = any (\os -> eSimplify (os M.! S) == (0 #)) (M.elems p)

-- | Checks whether there are any channels without receive operations.
-- If there are no receive operations and partial deadlocks are considered guaranteed,
-- likely the channel escapes the fragment scope, or is used as a free variable in a thunk,
-- (i.e. we are dealing with a false positive).
noReceivesFound :: Encoding -> Bool
noReceivesFound Encoding { comprojection = p } = any (\os -> eSimplify (os M.! R) == (0 #)) (M.elems p)

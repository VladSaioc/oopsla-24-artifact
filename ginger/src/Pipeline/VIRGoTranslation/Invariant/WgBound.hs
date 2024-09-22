module Pipeline.VIRGoTranslation.Invariant.WgBound (wgBounds) where

import Data.Set qualified as S

import Backend.Ast
import Backend.Utilities
import Pipeline.VIRGoTranslation.Summary.WgOp

{- | Composes all WaitGroup bound invariants. WaitGroup counters must never turn negative.

Depends on: 𝑊

Produces:

> ∀ w ∈ 𝑊. w ≥ 0
-}
wgBounds :: 𝑊 -> [Exp]
wgBounds = map wgBound . S.toList

{- | Constructs a WaitGroup bound invariant. WaitGroup counters must never turn negative.
Depends on: w ∈ 𝑊

Produces:

> w ≥ 0
-}
wgBound :: String -> Exp
wgBound w = (w @) :>= (0 #)

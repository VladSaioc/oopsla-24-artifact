module Pipeline.VIRGoTranslation.Summary.Chan where

import Backend.Ast qualified as T
import VIRGo.Ast
import Pipeline.VIRGoTranslation.Exps
import Pipeline.VIRGoTranslation.Utilities
import Utilities.Collection

-- | An alias for the type of concurrency primitive names, denoted as strings.
-- Its purpose is to shorten type definitions involving concurrency primitives.
type 𝐶 = String

-- | The type of channel capacity environments, connecting channel names to capacity expressions.
type 𝛫 = 𝐶 ↦ T.Exp

-- | Channel declaration summaries.
data 𝒞 = 𝒞
  { -- | Process of channel operation
    cP :: P,
    -- | Channel name
    c𝐶 :: 𝐶,
    -- | Capacity expression
    cCap :: T.Exp,
    -- | Program point
    c𝑛 :: 𝑁
  } deriving (Eq, Read)

{- | Aggregate all channel declaration summaries from a program.
Produces a list of channel declaration summaries, including the channel name,
process id, capacity expression, and program point.
Depends on: p, 𝑆

Produces:

> { 𝒞 {p, c, e, 𝑛} }
-}
chandefs :: 𝑆 -> [𝒞]
chandefs = programToCollection processChandefs

-- | Collect all channel declarations found in a process
processChandefs :: 𝛬 -> 𝑆 -> [𝒞]
processChandefs 𝛬 { p, 𝑛 } = \case
  Def (Chan c e) ->
    let c' = 𝒞 {
        cP = p,
        c𝐶 = c,
        cCap = parseExp e,
        c𝑛 = 𝑛
      }
     in [c']
  _ -> []

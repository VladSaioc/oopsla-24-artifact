module Pipeline.VIRGoTranslation.Summary.Summary where

import VIRGo.Ast
import Pipeline.VIRGoTranslation.Summary.Chan
import Pipeline.VIRGoTranslation.Summary.CommOp
import Pipeline.VIRGoTranslation.Summary.Go
import Pipeline.VIRGoTranslation.Summary.If
import Pipeline.VIRGoTranslation.Summary.Loop
import Pipeline.VIRGoTranslation.Summary.Return
import Pipeline.VIRGoTranslation.Summary.WgOp
import Pipeline.VIRGoTranslation.Utilities
import Utilities.Collection

-- | All statement summaries in the program.
data ℳ = ℳ
  { -- | Channel definition summaries
    cs :: [𝒞],
    -- | Channel outside-loop operation summaries
    os :: P ↦ (𝐶 ↦ 𝒪s),
    -- | WaitGroup outside-loop operation summaries
    ws :: P ↦ (𝐶 ↦ 𝒲s),
    -- | Go statement summaries
    gs :: [𝒢],
    -- | If statement summaries
    is :: [ℐ],
    -- | Loop summaries
    ls :: [ℒ],
    -- | Return statement summaries
    rs :: [ℛ]
  }

getSummaries :: 𝑆 -> ℳ
getSummaries p =
  ℳ
    {
      cs = chandefs p,
      os = noloopPsChanInsns p,
      ws = noloopPsWgInsns p,
      gs = gos p,
      is = ifs p,
      ls = loops p,
      rs = returns p
    }

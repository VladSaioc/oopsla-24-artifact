module Pipeline.VIRGoTranslation.Summary.Return where

import VIRGo.Ast
import Pipeline.VIRGoTranslation.Utilities

-- | Return instruction summary.
data ℛ = ℛ
  { -- | Process ID of return statement.
    rP :: P,
    -- | Program point
    r𝑛 :: 𝑁
  } deriving Eq

instance Show ℛ where
  -- PID: return 𝑛
  show ℛ {rP = p, r𝑛 = 𝑛} = unwords [show p ++ ":", "return", show 𝑛]

-- | Collect all return statements in the program.
-- Assume that all return statements are initially guarded by true.
returns :: 𝑆 -> [ℛ]
returns = programToCollection processReturns

-- | Collect all return statements found in a process.
processReturns :: 𝛬 -> 𝑆 -> [ℛ]
processReturns 𝜆 = \case
  -- Return statements only increment the program counter.
  Return -> [ℛ {r𝑛 = 𝑛 𝜆, rP = p 𝜆}]
  _ -> []

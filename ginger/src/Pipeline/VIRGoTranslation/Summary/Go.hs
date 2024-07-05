module Pipeline.VIRGoTranslation.Summary.Go where

import VIRGo.Ast
import Pipeline.VIRGoTranslation.Utilities

-- | Go instruction summary.
data 𝒢 = 𝒢
  { -- | Process ID of return statement.
    gP :: P,
    -- | Process ID of child goroutine.
    gP' :: P,
    -- | Program point
    g𝑛 :: 𝑁
  } deriving Eq

instance Show 𝒢 where
  -- PID: return 𝑛
  show 𝒢 {gP = p, gP' = p', g𝑛 = 𝑛} = unwords [show p ++ ": [" ++ show 𝑛 ++ "]", "go {", show p',"}"]

-- | Collect all if statements found in the program.
gos :: 𝑆 -> [𝒢]
gos = programToCollection processGos

-- | Collect all if statement found in a process.
processGos :: 𝛬 -> 𝑆 -> [𝒢]
processGos 𝛬 { p, 𝑛, nextp } = \case
  Go {} ->
    let g =
          𝒢
            { -- Go process
              gP = p,
              -- Child goroutine
              gP' = nextp,
              -- Program point
              g𝑛 = 𝑛
            }
     in [g]
  _ -> []

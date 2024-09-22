module Pipeline.VIRGoTranslation.Summary.If where

import Backend.Ast
import VIRGo.Ast qualified as I
import Pipeline.VIRGoTranslation.Exps (parseExp)
import Pipeline.VIRGoTranslation.Utilities
import Utilities.PrettyPrint

-- | Conditional statement summary.
data ℐ = ℐ
  { -- | Process of if statement
    iP :: P,
    -- | If guard
    iGuard :: Exp,
    -- | Guard program point
    i𝑛 :: 𝑁,
    -- | Else program point
    iElse :: 𝑁,
    -- | Exit program point
    iExit :: 𝑁
  } deriving Eq

instance Show ℐ where
  show ℐ {iP = p, iGuard = g, i𝑛 = 𝑛₁, iElse = 𝑛₂, iExit = 𝑛₃} =
    multiline
      -- PID: for x (lo .. hi) <n₁ --> n₂>
      [ unwords
          [ show p ++ ":",
            show 𝑛₁ ++ ": if",
            "(" ++ prettyPrint 0 g ++ ")",
            "<" ++ "-->" ++ show 𝑛₂ ++ "; -->" ++ show 𝑛₃ ++ ">"
          ]
      ]

-- | Collect all if statements found in the program.
ifs :: I.𝑆 -> [ℐ]
ifs = programToCollection processIfs

-- | Collect all if statement found in a process.
processIfs :: 𝛬 -> I.𝑆 -> [ℐ]
processIfs 𝛬 { p, 𝑛 } = \case
  I.If e s₁ s₂ ->
    let -- Process if branches continuation points.
        𝑛₁ = 𝑛 + 1 + I.ppOffset s₁
        𝑛₂ = 𝑛₁ + 1 + I.ppOffset s₂
        l =
          ℐ
            { -- Loop process
              iP = p,
              -- Parse lower bound expression
              iGuard = parseExp e,
              -- Guard is at the conditional entry program point
              i𝑛 = 𝑛,
              -- Else branch program point
              iElse = 𝑛₁ + 1,
              -- Conditional exit program point
              iExit = 𝑛₂
            }
     in [l]
  _ -> []

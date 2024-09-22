module Pipeline.VIRGoTranslation.Clauses.Utilities where

import Backend.Ast

-- | Creates a call to the "iter" function on loop bounds.
--
-- Produces:
--
-- > iter(lo,hi)
iterations :: Exp -> Exp -> Exp
iterations lo hi = Call 𝑥iter [lo, hi]

-- | The "iter" Dafny variable name as a pattern.
pattern 𝑋iter :: String
pattern 𝑋iter = "iter"

-- | The "iter" Dafny variable name as a value.
𝑥iter :: String
𝑥iter = "iter"

-- | The "step" Dafny variable name as a pattern.
pattern 𝑋step :: String
pattern 𝑋step = "step"

-- | The "step" Dafny variable name as a value.
𝑥step :: String
𝑥step = "step"

-- | The "fuel" Dafny variable name as a pattern.
pattern 𝑋fuel :: String
pattern 𝑋fuel = "fuel"

-- | The "fuel" Dafny variable name as a value.
𝑥fuel :: String
𝑥fuel = "fuel"

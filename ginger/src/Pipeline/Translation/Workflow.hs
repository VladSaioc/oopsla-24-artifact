module Pipeline.Translation.Workflow (promelaToGo, goToVIRGo) where

import Go.Ast (Prog)
import Go.GoForCommute (goForCommute)
import Go.Simplifier qualified as S (simplify)
import Go.ZipCases (zipCases)
import VIRGo.Ast
import VIRGo.Simplifier qualified as S' (simplify)
import Pipeline.Sanity.CallgraphOk (noRecursion)
import Pipeline.Sanity.GoAllowed (allowed)
import Pipeline.Translation.AlphaConversion (alphaConvert)
import Pipeline.Translation.GoToVIRGo (getIR)
import Pipeline.Translation.Metrics
import Pipeline.Translation.PromelaToGo (getGo)
import Promela.Ast (Spec)
import Utilities.Err

-- | Convert Promela programs to simple Go.
-- Can fail if the Promela program features recursion.
promelaToGo :: Spec -> Err Prog
promelaToGo p = do
  let p' = alphaConvert p
  _ <- noRecursion p'
  getGo p'

-- | Convert simple Go to VIRGo.
-- Can fail if the simple Go program uses unsupported constructs,
-- or supported constructs in unsupported ways.
goToVIRGo :: Prog -> Err (𝑆, TranslationMetrics)
goToVIRGo g = do
  let zimplify g1 =
        let g' = (zipCases . S.simplify) g1
          in if g1 == g' then g' else zimplify g'
  let g1 = zimplify g
  let g' = goForCommute g1
  _ <- allowed g'
  ir <- getIR g'
  return (S'.simplify ir, goForCommuteMetrics g1)

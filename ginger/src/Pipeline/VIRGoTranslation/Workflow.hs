module Pipeline.VIRGoTranslation.Workflow (virgoToBackend) where

import VIRGo.Ast
import VIRGo.SanityCheck (sanityCheck)
import VIRGo.Simplifier (simplify)
import Pipeline.VIRGoTranslation.Clauses.CommPrecondition
import Pipeline.VIRGoTranslation.Clauses.WgPrecondition
import Pipeline.VIRGoTranslation.Clauses.Postcondition (postcondition)
import Pipeline.VIRGoTranslation.Close
import Pipeline.VIRGoTranslation.Encoding
import Pipeline.VIRGoTranslation.Context.Capacity (caps)
import Pipeline.VIRGoTranslation.Context.Reachability (reachability)
import Pipeline.VIRGoTranslation.Context.TypeInference (typesAndFvs)
import Pipeline.VIRGoTranslation.Context.WaitGroups (wgnames)
import Pipeline.VIRGoTranslation.Processes (procs)
import Pipeline.VIRGoTranslation.Summary.Summary
import Utilities.Err

-- | Convert VIRGo program to back-end program. May fail.
virgoToBackend :: 𝑆 -> Err Encoding
virgoToBackend p' = do
  let p = simplify p'
  _ <- sanityCheck p
  (𝛾, ts) <- typesAndFvs p
  let 𝜅 = caps p
  let 𝜓 = reachability p
  let 𝜉 = procs 𝜅 p
  let 𝓂 = getSummaries p
  return $ Encoding
    { prog = p,
      conditions = 𝜓,
      capacities = 𝜅,
      waitgroups = wgnames p,
      typeenv = 𝛾,
      typevars = ts,
      processes = 𝜉,
      summaries = 𝓂,
      comprojection = projectedCommunication 𝜓 𝓂,
      wgprojection = projectedConcurrency 𝜓 𝓂,
      closes = closingChannels p,
      post = postcondition (gs 𝓂)
    }

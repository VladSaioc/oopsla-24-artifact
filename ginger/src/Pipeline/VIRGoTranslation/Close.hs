module Pipeline.VIRGoTranslation.Close where

import Data.Map qualified as M
import Data.Set qualified as S

import Backend.Ast
import Backend.Simplifier
import Backend.Utilities
import VIRGo.Ast
import VIRGo.Utilities
import Pipeline.VIRGoTranslation.Summary.Chan
import Pipeline.VIRGoTranslation.Utilities
import Utilities.Collection

closingChannels :: 𝑆 -> S.Set 𝐶
closingChannels = programToCollection (const $ \case
  Close c -> S.singleton c
  _ -> S.empty)

closingChannelMsg :: 𝐶 ↦ (CommOpType ↦ Exp) -> 𝐶 -> [String]
closingChannelMsg ops c = case M.lookup c ops of
  Nothing -> pure $ unwords [c, "does not need to be closed."]
  Just chops -> case M.lookup S chops of
    Just sends | containsConditionals (eSimplify sends) ->
      pure $ unwords [c, "closing is possibly conditionally redundant."]
    _ -> case M.lookup R chops of
      Just recvs | containsConditionals (eSimplify recvs) ->
        pure $ unwords [c, "closing is possibly conditionally redundant."]
      _ -> pure $ unwords [c, "does not need to be closed."]

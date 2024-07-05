module Pipeline.VIRGoTranslation.Context.WaitGroups (wgnames) where

import Data.Set qualified as S

import VIRGo.Ast
import Pipeline.VIRGoTranslation.Utilities

-- | Extracts the names of all wait groups from a VIRGo program.
wgnames :: 𝑆 -> S.Set String
wgnames = programToCollection $ const (\case
  Def (Wg w) -> S.singleton w
  _ -> S.empty)

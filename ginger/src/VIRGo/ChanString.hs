module VIRGo.ChanString (canonicalForm, congruent) where

import Data.Maybe (isJust)
import Data.Set qualified as S

-- The canonical form of a channel string ρ removes duplicate neighbors.
canonicalForm :: Eq a => [a] -> [a]
canonicalForm = \case
  [] -> []
  [c1] -> [c1]
  c1 : c2 : s ->
    if c1 == c2
      then canonicalForm (c2 : s)
      else c1 : canonicalForm (c2 : s)

-- Checks the canonical form of a channel string ρ for duplicates.
validate :: Ord a => [a] -> Maybe [a]
validate s =
  let cs = canonicalForm s
      _validChStr h = \case
        [] -> True
        c : s' -> not (S.member c h) && _validChStr (S.insert c h) s'
   in if _validChStr S.empty cs
        then Just cs
        else Nothing

-- Check that the ordering of individual channels
-- is preserved between canonical channel strings ρ1 and ρ2
congruent :: Ord a => [a] -> [a] -> Bool
congruent cs1 cs2 =
  isJust
    ( do
        -- Get canonical form of ρ1
        s1 <- validate cs1
        -- Get canonical form of ρ2
        s2 <- validate cs2
        -- Check congruency by checking that:
        -- ∀c' ∈ suf(ρ2, c), c' ∉ pref(ρ1, c)
        let _congruent pcs = \case
              [] -> True
              c : s1' ->
                -- Get suf(ρ2, c)
                let cont = \case
                      [] -> []
                      (c' : s') ->
                        if c == c' then s' else cont s'
                 in -- Check that  ∀c' ∈ suf(ρ2, c), c' ∉ pref(ρ1, c)
                    -- Recursively traverse ρ1, adding c to predecessors.
                    all (`S.notMember` pcs) (cont s2) && _congruent (S.insert c pcs) s1'
         in if _congruent S.empty s1
              then return True
              else Nothing
    )

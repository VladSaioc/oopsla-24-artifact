module Pipeline.Translation.Metrics (metrics₀, goForCommuteMetrics, TranslationMetrics) where

import Data.Maybe

import Go.Ast
import Utilities.Position

-- | Quantify translation metrics. For example,
-- the number of go-for translations performed, including
-- the number of sound translations.
data TranslationMetrics = TranslationMetrics {
  soundgofors :: Int,
  unsoundgofors :: Int
} deriving (Eq, Ord, Read)

instance Show TranslationMetrics where
  show TranslationMetrics {soundgofors, unsoundgofors} =
    unlines [
      "Performed the following transformations:",
      unwords ["Sound go-for commutes:", show soundgofors],
      unwords ["Unsound go-for commutes:", show unsoundgofors]
    ]

-- | Zero value of translation metrics collection.
metrics₀ :: TranslationMetrics
metrics₀ = TranslationMetrics {
  soundgofors = 0,
  unsoundgofors = 0
}

-- | Collects go-for commute bodies
goForCommuteMetrics :: Prog -> TranslationMetrics
goForCommuteMetrics (Prog ss) = passSpine metrics₀ ss

-- | Traverse the main path of the program.
passSpine :: TranslationMetrics -> [Pos Stmt] -> TranslationMetrics
passSpine m = \case
  [] -> m
  Pos _ s : ss -> case s of
    Skip -> passSpine m ss
    Continue -> passSpine m ss
    Break -> passSpine m ss
    Return -> passSpine m ss
    Decl {} -> passSpine m ss
    As {} -> passSpine m ss
    Wgdef {} -> passSpine m ss
    Add {} -> passSpine m ss
    Wait {} -> passSpine m ss
    Chan {} -> passSpine m ss
    Atomic {} -> passSpine m ss
    Close {} -> passSpine m ss
    Block ss' ->
      let m1 = passSpine m ss'
       in passSpine m1 ss'
    For _ _ _ _ ss' ->
      let m' = pass m ss'
       in passSpine m' ss
    If _ ss1 ss2 ->
      let m1 = passSpine m ss1
          m2 = passSpine m1 ss2
       in passSpine m2 ss
    Select cs d ->
      let m1 = maybe m (passSpine m) d
          m2 = foldl (\m' (_, ss') -> passSpine m' ss') m1 cs
       in passSpine m2 ss
    While {} -> passSpine m ss
    Go ss' ->
      let m1 = passSpine m ss'
       in passSpine m1 ss'

-- | Inspects the body of a for loop to check whether the go-for commute
-- was a sound or unsound transformation.
pass :: TranslationMetrics -> [Pos Stmt] -> TranslationMetrics
pass m = \case
  [] -> m
  Pos _ s : ss ->
    let cont = pass m ss
    in case s of
      Skip -> cont
      Continue -> cont
      Break -> cont
      Return -> cont
      Decl {} -> cont
      As {} -> cont
      Wgdef {} -> cont
      Add {} -> cont
      Wait {} -> cont
      Chan {} -> cont
      Atomic {} -> cont
      Close {} -> cont
      Block ss' ->
        let m' = pass m ss'
        in pass m' ss
      If {} -> cont
      Select {} -> cont
      For {} -> cont
      While {} -> cont
      Go ss' ->
          let Ctx { unsound } = checkGoBody Ctx {op = Nothing, unsound = False} ss'
              m' = m { soundgofors = soundgofors m + (if unsound then 0 else 1),
                        unsoundgofors = unsoundgofors m + (if unsound then 1 else 0)
                      }
          in pass m' ss

-- | Context for checking the soundness of a go-for transformation.
data Ctx = Ctx {
  -- | Populated with the name of the channel operated on.
  -- All channel operations must be the same as the populated one for
  -- the loop to be sound.
  op :: Maybe CommOp,
  -- | Set to true if an unsound loop transformation is suspected.
  unsound :: Bool
}

checkGoBody :: Ctx -> [Pos Stmt] -> Ctx
checkGoBody c@Ctx { op, unsound } = \case
  [] -> c
  Pos _ s : ss ->
    let cont = checkGoBody c ss
    in if unsound
        then c
      else case s of
        Atomic o ->
          let c' = c { op = Just $ fromMaybe o op,
            unsound = isJust op && Just o /= op }
          in checkGoBody c' ss
        Skip -> cont
        Continue -> cont
        Break -> cont
        Return -> cont
        Decl {} -> cont
        As {} -> cont
        Wgdef {} -> cont
        Add {} -> cont
        Wait {} -> cont
        Chan {} -> cont
        Close {} -> cont
        Block ss' ->
          let c' =  checkGoBody c ss'
           in checkGoBody c' ss
        If {} -> cont
        Select {} -> cont
        For {} -> cont
        While {} -> cont
        Go {} -> cont

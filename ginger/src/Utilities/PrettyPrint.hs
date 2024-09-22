module Utilities.PrettyPrint where

import Data.List (intercalate)

-- | PrettyPrint is used for indented unparsing of syntax trees.
class PrettyPrint a where
  prettyPrint :: Int -> a -> String

-- | Intercalates multiple strings with newline.
multiline :: [[Char]] -> [Char]
multiline = intercalate "\n"

-- | Prepends an indentation of n double spaces.
indent :: Int -> String -> String
indent n = (concat (replicate n "  ") ++)

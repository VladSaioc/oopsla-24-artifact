module Utilities.Args
  (hasIRFlag,
  hasColor,
  skipVerification,
  getFilePath,
  getResultDir,
  getDafnyPath) where

import Data.List (intercalate, isPrefixOf)
import Data.List.Split (splitOn)
import Utilities.Err

_DIST :: String
_DIST = "output"
_DAFNY :: String
_DAFNY = "dafny"

-- | Prefixes a file path with ".\/" if it is not absolute.
formatPath :: String -> String -> String
formatPath fallback path =
  let isAbs = "/" `isPrefixOf` path
   in if "-" `isPrefixOf` path
    then fallback
    else if isAbs
      then path
      else "./" ++ path

-- | Check whether a flag `f` was provided as a command line flag.
hasFlag :: String -> [String] -> Bool
hasFlag f args =case args of
  [] -> False
  a : as -> "-" ++ f == a || hasFlag f as

-- | Check whether `-color` was provided as a command line argument.
-- When set, the tool prints colorful output (useful for the console).
hasColor :: [String] -> Bool
hasColor = hasFlag "color"

-- | Checks whether `-skip-verification` was provided as a command line argument.
-- When set, the tool skips verification attempts, and instead only prints the output file.
skipVerification :: [String] -> Bool
skipVerification = hasFlag "skip-verification"

-- | Check whether `-virgo` was provided as a command line argument.
-- When set, the tool uses the intermediate representation parser instead of the Promela parser.
hasIRFlag :: [String] -> Bool
hasIRFlag = hasFlag "virgo"

-- | Get file path of analyzed file.
getFilePath :: [String] -> Err String
getFilePath = \case
  [] -> Bad "No file provided."
  [a] ->
    if "-" `isPrefixOf` a
      then Bad "No file provided."
      else return a
  _ : as -> getFilePath as

-- | Get result directory. Defaults to analyzed file parent directory.
getResultDir :: [String] -> String
getResultDir args =
  let dir = case getFilePath args of
        Ok filePath ->
          let -- Split the file path by '/'
              parts = splitOn "/" filePath
              -- Discard the last element of the path (the name of the file)
              dirPath = intercalate "/" (take (length parts - 1) parts)
           in formatPath _DIST dirPath
        Bad _ -> _DIST
   in case args of
        [] -> dir
        [_] -> dir
        "-output-dir" : a : _ -> formatPath _DIST a
        _ : as -> getResultDir as

-- | Extract user provided path to Dafny executable. Defaults to `dafny`.
getDafnyPath :: [String] -> String
getDafnyPath = \case
  [] -> _DAFNY
  [_] -> _DAFNY
  "-dafny" : exe : _ -> formatPath _DAFNY exe
  _ : args -> getDafnyPath args

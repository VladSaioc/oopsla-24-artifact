module Utilities.Color where

import System.Console.ANSI

import Utilities.Args (hasColor)

-- | Print a colored message, if the `-color` flag is set to true.
printColoredMsg :: [String] -> Color -> String -> IO ()
printColoredMsg args c msg = do
    setSGR [SetColor Foreground Vivid c | hasColor args]
    putStr msg
    setSGR [Reset | hasColor args]

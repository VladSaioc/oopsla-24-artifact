module Main where

import Control.Monad (unless)
import System.Environment

import Go.Profiler (profileProgram)
import VIRGo.GetAst qualified as I
import VIRGo.Profiler (getVirgoParametricity)
import VIRGo.Simplifier (simplify)
import VIRGo.Utilities (interesting)
import Pipeline.Translation.Metrics (metrics₀)
import Pipeline.Translation.Workflow (promelaToGo, goToVIRGo)
import Pipeline.Verification.Runner (verify)
import Promela.GetAst qualified as P
import Utilities.Args
import Utilities.Err

main :: IO ()
main = do
  args <- getArgs
  filePath <- case getFilePath args of
    Ok filePath -> return filePath
    Bad _ -> ioError $ userError "Give me a Promela/VIRGo file."
  source <- readFile filePath
  ir <-
        if hasIRFlag args
          then
              let ir = do
                    source' <- I.getAst source
                    return (source', metrics₀)
               in return ir
          else do
              let mg = do
                    prom <- P.getAst source
                    promelaToGo prom
              case mg of
                Ok g -> do
                  let msg = profileProgram g
                  putStrLn $ unlines ["Profiling Go program parametricity:", msg]
                  return (goToVIRGo g)
                Bad msg -> return $ Bad msg
  p <- case ir of
    Ok (ir', m) -> do
      let ir'' = simplify ir'
      unless (interesting ir'') $ ioError $ userError "Program is not interesting."
      putStrLn "\n"
      putStrLn "VIRGo translation:"
      print ir''
      unless (m == metrics₀) $ print m
      return ir''
    Bad msg -> ioError $ userError $ "VIRGo translation failed: " ++ msg
  verify args filePath p

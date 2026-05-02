-- | Driver for the Agent Workflow Language: parse a source file, run
--   the big-step interpreter, and print the resulting environments.
module Main where

import qualified Data.Map.Strict     as Map
import           System.Environment  (getArgs)
import           System.Exit         (exitFailure)

import           Interpreter
import           Parser

main :: IO ()
main = do
  args <- getArgs
  (src, contents) <- case args of
    [path] -> (,) path        <$> readFile path
    []     -> (,) "<stdin>"   <$> getContents
    _      -> putStrLn "usage: awl [program.awl]" >> exitFailure
  case parseProgram src contents of
    Left  err  -> do
      putStrLn "parse error:"
      print err
      exitFailure
    Right prog -> do
      r <- evalStmt initialState prog
      case r of
        EOk st -> do
          putStrLn "[ok]"
          putStrLn "config:"
          mapM_ (\(k,v) -> putStrLn ("  " ++ k ++ " = " ++ showVal v))
                (Map.toAscList (sConfig st))
          putStrLn "vars:"
          mapM_ (\(k,v) -> putStrLn ("  " ++ k ++ " = " ++ showVal v))
                (Map.toAscList (sVar st))
        EErr v -> do
          putStrLn "[error]"
          putStrLn (showVal v)
          exitFailure

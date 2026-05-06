-- | Driver for the Agent Workflow Language: parse a source file, run
--   the big-step interpreter, and print the resulting environments.
module Main where

import qualified Data.Map.Strict     as Map
import           System.Environment  (getArgs)
import           System.Exit         (exitFailure)

import           Interpreter
import           Parser
import           Syntax       (Value)

data OutputMode = FullDump | Quiet | ResultOnly | ResultJson
  deriving Eq

main :: IO ()
main = do
  args <- getArgs
  (mode, mPath) <- case parseArgs args of
    Right parsed -> return parsed
    Left msg     -> putStrLn msg >> exitFailure
  (src, contents) <- case mPath of
    Just path -> (,) path        <$> readFile path
    Nothing   -> (,) "<stdin>"   <$> getContents
  case parseProgram src contents of
    Left  err  -> do
      putStrLn "parse error:"
      print err
      exitFailure
    Right prog -> do
      r <- evalStmt initialState prog
      case r of
        EOk st -> printSuccess mode st
        EErr v -> do
          putStrLn "[error]"
          putStrLn (showVal v)
          exitFailure

parseArgs :: [String] -> Either String (OutputMode, Maybe FilePath)
parseArgs = go FullDump Nothing
  where
    go mode path [] = Right (mode, path)
    go _ _ ("--help":_) = Left usage
    go _ path ("--quiet":rest) = go Quiet path rest
    go _ path ("--result-only":rest) = go ResultOnly path rest
    go _ path ("--result-json":rest) = go ResultJson path rest
    go _ _ (arg:_) | take 2 arg == "--" = Left usage
    go mode Nothing (path:rest) = go mode (Just path) rest
    go _ (Just _) (_:_) = Left usage

usage :: String
usage = "usage: awl [--quiet|--result-only|--result-json] [program.awl]"

printSuccess :: OutputMode -> EvalState -> IO ()
printSuccess FullDump st = do
  putStrLn "[ok]"
  putStrLn "config:"
  mapM_ (\(k,v) -> putStrLn ("  " ++ k ++ " = " ++ showVal v))
        (Map.toAscList (sConfig st))
  putStrLn "vars:"
  mapM_ (\(k,v) -> putStrLn ("  " ++ k ++ " = " ++ showVal v))
        (Map.toAscList (sVar st))
printSuccess Quiet _ = return ()
printSuccess ResultOnly st = printResult showVal st
printSuccess ResultJson st = printResult valueToJsonText st

printResult :: (Value -> String) -> EvalState -> IO ()
printResult render st =
  case Map.lookup "result" (sVar st) of
    Just v  -> putStrLn (render v)
    Nothing -> do
      putStrLn "[error]"
      putStrLn "result variable not found"
      exitFailure

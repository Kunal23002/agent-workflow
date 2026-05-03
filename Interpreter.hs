{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Big-step interpreter for the Agent Workflow Language (section 3 of
--   the spec).  Each Haskell case in 'evalExpr' / 'evalStmt' corresponds
--   to one inference rule (E-Const, …, S-Try-Catch).
module Interpreter
  ( -- * Driving the evaluator
    EvalState(..)
  , initialState
  , evalStmt
  , EResult(..)
    -- * Pretty printing
  , showVal
  ) where

import           Control.Exception          (SomeException, try)
import           Data.Char                  (isSpace, toLower, toUpper)
import           Data.List                  (intercalate, isInfixOf, nub)
import qualified Data.Map.Strict            as Map
import           Data.Map.Strict            (Map)
import qualified Data.Aeson                 as A
import qualified Data.Aeson.KeyMap          as KM
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Network.HTTP.Simple
import           System.Environment         (lookupEnv)

import           Syntax

------------------------------------------------------------------------
--  Environments
------------------------------------------------------------------------

-- | σ : variable environment
type VarEnv    = Map String Value
-- | Δ : agent environment
type AgentEnv  = Map String AgentDef
-- | Γ : configuration environment
type ConfigEnv = Map String Value

-- | The runtime form of an agent declaration.
data AgentDef
  = ADBackend Backend                     -- backend(b)
  | ADFixed   Kind                        -- fixed(k)
  | ADCustom  Value String                -- custom(prompt, m)
  deriving (Eq, Show)

data EvalState = EvalState
  { sVar    :: VarEnv
  , sAgent  :: AgentEnv
  , sConfig :: ConfigEnv
  } deriving Show

initialState :: EvalState
initialState = EvalState Map.empty Map.empty Map.empty

-- | An evaluation either yields a result or an error value.  This is
--   how the spec's @err(v)@ values propagate (rule E-Prop).
data EResult a = EOk a | EErr Value
  deriving (Eq, Show)

type EvalIO a = IO (EResult a)

okE :: a -> EvalIO a
okE  = return . EOk

errE :: Value -> EvalIO a
errE = return . EErr

-- | Bind for the @EvalIO@ "monad" without having to wrap in a transformer.
bindE :: EvalIO a -> (a -> EvalIO b) -> EvalIO b
bindE m f = do
  r <- m
  case r of
    EOk a  -> f a
    EErr v -> return (EErr v)
infixl 1 `bindE`

------------------------------------------------------------------------
--  Expression rules
------------------------------------------------------------------------

evalExpr :: EvalState -> Expr -> EvalIO Value
-- (E-Const)
evalExpr _  (EConst v) = okE v
-- (E-Var)
evalExpr st (EVar  x)  = case Map.lookup x (sVar st) of
  Just v  -> okE v
  Nothing -> errE (VString ("unbound variable: " ++ x))
-- (E-Proj)
evalExpr st (EProj e f) =
  evalExpr st e `bindE` \v -> case v of
    VRecord fs -> case lookup f fs of
      Just v' -> okE v'
      Nothing -> errE (VString ("no such field: " ++ f))
    _ -> errE (VString ("field access on non-record: ." ++ f))
-- (E-Rec)
evalExpr st (ERecord fs) = go fs []
  where
    go []         acc = okE (VRecord (reverse acc))
    go ((f,e):rs) acc = evalExpr st e `bindE` \v -> go rs ((f,v):acc)
-- (E-Op)
evalExpr st (EBin op e1 e2) =
  evalExpr st e1 `bindE` \v1 ->
  evalExpr st e2 `bindE` \v2 ->
    case applyBinOp op v1 v2 of
      Just v  -> okE v
      Nothing -> errE (VString ("type error in operator " ++ show op))
-- (E-Agent-*) selected from the registered agent definition
evalExpr st (ECall name es) = case Map.lookup name (sAgent st) of
  Nothing  -> errE (VString ("unknown agent: " ++ name))
  Just def -> evalArgs st es `bindE` \vs -> applyAgent st def vs

evalArgs :: EvalState -> [Expr] -> EvalIO [Value]
evalArgs _  []     = okE []
evalArgs st (e:es) =
  evalExpr st e  `bindE` \v  ->
  evalArgs st es `bindE` \vs -> okE (v:vs)

-- | The semantic content of @⊕(v₁, v₂)@.
applyBinOp :: BinOp -> Value -> Value -> Maybe Value
applyBinOp OpAdd (VNumber a) (VNumber b) = Just (VNumber (a + b))
applyBinOp OpAdd (VString a) (VString b) = Just (VString (a ++ b))
applyBinOp OpSub (VNumber a) (VNumber b) = Just (VNumber (a - b))
applyBinOp OpMul (VNumber a) (VNumber b) = Just (VNumber (a * b))
applyBinOp OpDiv (VNumber a) (VNumber b) = Just (VNumber (a / b))
applyBinOp OpEq  a            b           = Just (VBool (a == b))
applyBinOp OpNeq a            b           = Just (VBool (a /= b))
applyBinOp OpGt  (VNumber a) (VNumber b) = Just (VBool (a >  b))
applyBinOp OpLt  (VNumber a) (VNumber b) = Just (VBool (a <  b))
applyBinOp OpGte (VNumber a) (VNumber b) = Just (VBool (a >= b))
applyBinOp OpLte (VNumber a) (VNumber b) = Just (VBool (a <= b))
applyBinOp OpAnd (VBool   a) (VBool   b) = Just (VBool (a && b))
applyBinOp OpOr  (VBool   a) (VBool   b) = Just (VBool (a || b))
applyBinOp _ _ _                         = Nothing

------------------------------------------------------------------------
--  Agent dispatch
------------------------------------------------------------------------

applyAgent :: EvalState -> AgentDef -> [Value] -> EvalIO Value
applyAgent st (ADBackend b)        vs  = runBackend (sConfig st) b vs
applyAgent _  (ADFixed   k)        vs  = okE (runFixed k vs)
applyAgent st (ADCustom  pmpt m)  [v]  = runLLM (sConfig st) m pmpt v
applyAgent _  (ADCustom  _    _)   _   =
  errE (VString "CustomAI agent expects exactly one argument")

-- | The four backends in §1 ::= python | http | llm | mock.
--
--   The python/http dispatchers are stubs: rather than make a network
--   call we synthesise a record describing the call so the semantics
--   are observable in tests.  The llm dispatcher and CustomAI optionally
--   route to a real Claude call when @config { real_llm = true }@ is set
--   in the program AND ANTHROPIC_API_KEY is present in the environment;
--   otherwise it falls back to the deterministic stub.
runBackend :: ConfigEnv -> Backend -> [Value] -> EvalIO Value
runBackend _   (BMock v)   _  = okE v
runBackend _   (BPython f) vs = do
  putStrLn $ "[python " ++ f ++ "] " ++ showArgs vs
  okE (synthRecord "python" f vs)
runBackend _   (BHttp u) vs = do
  putStrLn $ "[http   " ++ u ++ "] " ++ showArgs vs
  okE (synthRecord "http" u vs)
runBackend cfg (BLlm m) vs = do
  putStrLn $ "[llm    " ++ m ++ "] " ++ showArgs vs
  let prompt = intercalate "\n\n" (map valueText vs)
      mkResponse txt = VRecord
        [ ("backend",  VString "llm")
        , ("model",    VString m)
        , ("prompt",   VString prompt)
        , ("response", VString txt)
        ]
  liveOk <- liveLlmEnabled cfg
  case liveOk of
    Nothing  -> okE (mkResponse ("(simulated " ++ m ++ " response)"))
    Just key -> do
      r <- callClaude key m prompt
      case r of
        Right txt -> okE (mkResponse txt)
        Left err  -> errE (VString ("LLM call failed: " ++ err))

-- | (E-Agent-Custom) — optional real call when opted in via config.
runLLM :: ConfigEnv -> String -> Value -> Value -> EvalIO Value
runLLM cfg model prompt input = do
  putStrLn $ "[customAI " ++ model ++ "] prompt=" ++ showVal prompt
                              ++ " input=" ++ showVal input
  liveOk <- liveLlmEnabled cfg
  case liveOk of
    Nothing  -> okE (VRecord
      [ ("model",  VString model)
      , ("prompt", prompt)
      , ("input",  input)
      , ("output", VString ("(simulated " ++ model ++ " response)"))
      ])
    Just key -> do
      let combined = valueText prompt ++ "\n\n" ++ valueText input
      r <- callClaude key model combined
      case r of
        Right txt -> okE (VRecord
          [ ("model",  VString model)
          , ("prompt", prompt)
          , ("input",  input)
          , ("output", VString txt)
          ])
        Left err  -> errE (VString ("CustomAI call failed: " ++ err))

-- | Returns @Just apiKey@ when the program opted in via
--   @config { real_llm = true }@ and ANTHROPIC_API_KEY is set;
--   otherwise @Nothing@ (use the stub).
liveLlmEnabled :: ConfigEnv -> IO (Maybe String)
liveLlmEnabled cfg = case Map.lookup "real_llm" cfg of
  Just (VBool True) -> lookupEnv "ANTHROPIC_API_KEY"
  _                 -> pure Nothing

-- | Single-shot Claude Messages API call. Raw HTTP — no Haskell SDK exists.
--   See https://docs.claude.com/en/api/messages
callClaude :: String -> String -> String -> IO (Either String String)
callClaude apiKey model prompt = do
  let body = A.object
        [ "model"      A..= model
        , "max_tokens" A..= (1024 :: Int)
        , "messages"   A..=
            [ A.object
                [ "role"    A..= ("user" :: T.Text)
                , "content" A..= prompt
                ]
            ]
        ]
  initReq <- parseRequest "POST https://api.anthropic.com/v1/messages"
  let req = setRequestHeader "x-api-key"         [BS.pack apiKey]
          $ setRequestHeader "anthropic-version" ["2023-06-01"]
          $ setRequestBodyJSON body initReq
  result <- try (httpLBS req) :: IO (Either SomeException (Response LBS.ByteString))
  case result of
    Left e     -> pure (Left ("network error: " ++ show e))
    Right resp ->
      let status = getResponseStatusCode resp
          rbody  = getResponseBody resp
      in if status /= 200
           then pure (Left ("HTTP " ++ show status ++ ": " ++ LBS.unpack rbody))
           else case A.eitherDecode rbody of
             Left  err -> pure (Left ("decode error: " ++ err))
             Right v   -> case extractText v of
               Just t  -> pure (Right t)
               Nothing -> pure (Left ("unexpected response shape: " ++ LBS.unpack rbody))

-- | Pull @content[0].text@ out of a Claude Messages API response.
extractText :: A.Value -> Maybe String
extractText (A.Object obj) = do
  contents <- KM.lookup "content" obj
  case contents of
    A.Array vec | not (V.null vec) ->
      case V.head vec of
        A.Object firstBlock -> case KM.lookup "text" firstBlock of
          Just (A.String t) -> Just (T.unpack t)
          _                 -> Nothing
        _ -> Nothing
    _ -> Nothing
extractText _ = Nothing

synthRecord :: String -> String -> [Value] -> Value
synthRecord backend target vs = VRecord
  [ ("backend", VString backend)
  , ("target",  VString target)
  , ("args",    VRecord (zip [ "arg" ++ show i | i <- [0 :: Int ..] ] vs))
  ]

-- | Stubs for the 18 fixed agent kinds.  They do not call any model;
--   they package their inputs into a structured response so workflows
--   are deterministic in tests.
runFixed :: Kind -> [Value] -> Value
runFixed Merger vs = VRecord
  [("merged", VRecord (zip [ "k" ++ show i | i <- [0::Int ..] ] vs))]
runFixed Ranker vs = VRecord
  [("ranked", VRecord (zip [ "k" ++ show i | i <- [0::Int ..] ] vs))]
runFixed k [v] = runFixedOne k v
runFixed k vs  = VRecord
  [ ("kind",   VString (show k))
  , ("inputs", VRecord (zip [ show (i :: Int) | i <- [0..] ] vs))
  ]

-- | Flatten a value to text for processing: strings unwrapped, everything
--   else rendered via 'showVal'.
valueText :: Value -> String
valueText (VString s) = s
valueText v           = showVal v

trimStr :: String -> String
trimStr = f . f where f = dropWhile isSpace . reverse

lowerStr :: String -> String
lowerStr = map toLower

capitalizeWords :: String -> String
capitalizeWords = unwords . map cap . words
  where
    cap []     = []
    cap (c:cs) = toUpper c : cs

splitOnChar :: Char -> String -> [String]
splitOnChar c s = case break (== c) s of
  (pre, [])     -> [pre]
  (pre, _:rest) -> pre : splitOnChar c rest

runFixedOne :: Kind -> Value -> Value
-- Each step references the actual goal; complexity = word count of goal.
runFixedOne Planner v =
  let goal = valueText v
      ws   = length (words goal)
  in VRecord
       [ ("goal",       v)
       , ("complexity", VNumber (fromIntegral ws))
       , ("steps", VRecord
           [ ("0", VString ("Define scope of: "       ++ goal))
           , ("1", VString ("Gather information for: " ++ goal))
           , ("2", VString  "Synthesise findings")
           , ("3", VString ("Review and finalise: "    ++ goal))
           ])
       ]
-- Split on '.' (sentences) into discrete tasks.
runFixedOne TaskSplitter v =
  let txt   = valueText v
      parts = filter (not . null) (map trimStr (splitOnChar '.' txt))
      tasks = if null parts then [txt] else parts
  in VRecord
       [ ("input", v)
       , ("count", VNumber (fromIntegral (length tasks)))
       , ("tasks", VRecord (zip [show (i :: Int) | i <- [0..]]
                                (map VString tasks)))
       ]
-- Synthesise three deterministic "hits" that incorporate the query string.
runFixedOne Searcher v =
  let q = valueText v
  in VRecord
       [ ("query", v)
       , ("count", VNumber 3)
       , ("hits", VRecord
           [ ("0", VString (q ++ " — primary source"))
           , ("1", VString (q ++ " — review article"))
           , ("2", VString (q ++ " — community discussion"))
           ])
       ]
-- Extract first 5 words of the source.
runFixedOne Extractor v =
  let txt   = valueText v
      first = take 5 (words txt)
  in VRecord
       [ ("source",     v)
       , ("extracted",  VString (unwords first))
       , ("word_count", VNumber (fromIntegral (length first)))
       ]
-- Real cleaning: trim whitespace, lowercase, collapse internal whitespace.
runFixedOne Cleaner v =
  let txt     = valueText v
      cleaned = unwords (words (lowerStr txt))
  in VRecord
       [ ("input",   v)
       , ("cleaned", VString cleaned)
       ]
-- Deduplicate words while preserving order.
runFixedOne Deduplicator v =
  let txt     = valueText v
      ws      = words txt
      uniq    = nub ws
      removed = length ws - length uniq
  in VRecord
       [ ("input",   v)
       , ("deduped", VString (unwords uniq))
       , ("removed", VNumber (fromIntegral removed))
       ]
runFixedOne Formatter v = VRecord [("formatted", VString (showVal v))]
-- Score derived from content: high by default, drops if negative keywords appear.
runFixedOne Critic v =
  let txt    = lowerStr (valueText v)
      ws     = length (words txt)
      hasNeg = any (`isInfixOf` txt) ["bad", "wrong", "broken", "fail", "error"]
      score  | hasNeg    = 0.4
             | ws == 0   = 0.0
             | otherwise = 0.9
      crit   | hasNeg    = "contains negative indicators"
             | ws == 0   = "empty input"
             | otherwise = "no obvious issues"
  in VRecord
       [ ("input",      v)
       , ("word_count", VNumber (fromIntegral ws))
       , ("critique",   VString crit)
       , ("score",      VNumber score)
       ]
-- Heuristic check: claims containing negation/falsehood markers fail.
runFixedOne FactChecker v =
  let txt = lowerStr (valueText v)
      neg = any (`isInfixOf` txt) [" not ", " never ", "false", "untrue"]
  in VRecord
       [ ("claim",    v)
       , ("verified", VBool (not neg))
       , ("reason",   VString (if neg then "contains negation/contradiction"
                                       else "no contradictions found"))
       ]
-- Confidence scales with content length, capped at 1.0.
runFixedOne ConfidenceEstimator v =
  let ws   = length (words (valueText v))
      conf = min 1.0 (fromIntegral ws / 30.0)
  in VRecord
       [ ("input",      v)
       , ("word_count", VNumber (fromIntegral ws))
       , ("confidence", VNumber conf)
       ]
runFixedOne Writer     v = VRecord [("draft",   VString ("draft about: " ++ showVal v))]
runFixedOne Summarizer v = VRecord [("summary", VString (truncate' 80 (showVal v)))]
-- Capitalise the first letter of each word.
runFixedOne Rewriter v =
  let txt = valueText v
  in VRecord
       [ ("original",  v)
       , ("rewritten", VString (capitalizeWords txt))
       ]
-- Valid iff the input has at least one non-whitespace character.
runFixedOne Validator v =
  let ok = not (null (trimStr (valueText v)))
  in VRecord
       [ ("valid",   VBool ok)
       , ("payload", v)
       , ("reason",  VString (if ok then "non-empty input" else "empty input"))
       ]
-- Block content containing well-known sensitive keywords.
runFixedOne Guardrail v =
  let txt       = lowerStr (valueText v)
      sensitive = ["password", "secret", "ssn", "credit card", "api_key"]
      hits      = filter (`isInfixOf` txt) sensitive
      safe      = null hits
  in VRecord
       [ ("safe",    VBool safe)
       , ("payload", v)
       , ("reason",  VString (if safe then "no sensitive keywords"
                                       else "contains: " ++ intercalate ", " hits))
       ]
runFixedOne Fallback v = VRecord
  [ ("input",    v)
  , ("fallback", VString "(no primary result, using fallback)")
  ]
-- Pick a route from keywords found in the input.
runFixedOne Router v =
  let txt   = lowerStr (valueText v)
      route | any (`isInfixOf` txt) ["error", "fail"]  = "error_handler"
            | any (`isInfixOf` txt) ["how", "what", "?"] = "qa"
            | any (`isInfixOf` txt) ["buy", "price", "order"] = "commerce"
            | otherwise = "default"
  in VRecord
       [ ("payload", v)
       , ("route",   VString route)
       ]
-- Merger / Ranker single-arg cases are unreachable (caught by runFixed above)
-- but kept here so the pattern match against Kind is exhaustive.
runFixedOne Merger v = VRecord [("merged", v)]
runFixedOne Ranker v = VRecord [("ranked", v)]

------------------------------------------------------------------------
--  Statement rules
------------------------------------------------------------------------

evalStmt :: EvalState -> Stmt -> EvalIO EvalState
-- (S-Config)
evalStmt st (SConfig fs) = go fs (sConfig st)
  where
    go []         cfg = okE st { sConfig = cfg }
    go ((c,e):rs) cfg =
      evalExpr st e `bindE` \v -> go rs (Map.insert c v cfg)
-- (S-Decl-Backend)
evalStmt st (SAgentBackend name b) =
  okE st { sAgent = Map.insert name (ADBackend b) (sAgent st) }
-- (S-Decl-Fixed)
evalStmt st (SAgentFixed name k) =
  okE st { sAgent = Map.insert name (ADFixed k) (sAgent st) }
-- (S-Decl-Custom)
evalStmt st (SAgentCustom name pe m) =
  evalExpr st pe `bindE` \pv ->
    okE st { sAgent = Map.insert name (ADCustom pv m) (sAgent st) }
-- (S-Let)
evalStmt st (SLet x e) =
  evalExpr st e `bindE` \v ->
    okE st { sVar = Map.insert x v (sVar st) }
-- (S-IfTrue) / (S-IfFalse)
evalStmt st (SIf c s1 s2) =
  evalExpr st c `bindE` \v -> case v of
    VBool True  -> evalStmt st s1
    VBool False -> evalStmt st s2
    _           -> errE (VString "if-condition is not a boolean")
-- (S-Seq)
evalStmt st (SSeq s1 s2) =
  evalStmt st s1 `bindE` \st' -> evalStmt st' s2
-- (S-Fail)
evalStmt st (SFail e) =
  evalExpr st e `bindE` errE
-- (S-Retry-OK | S-Retry-Step | S-Retry-Done)
evalStmt st (SRetry n s) = loop n
  where
    loop 0 = evalStmt st s                         -- final attempt
    loop k = do
      r <- evalStmt st s
      case r of
        EOk st' -> okE st'
        EErr _  -> loop (k - 1)
-- (S-Try-OK | S-Try-Catch)
evalStmt st (STryCatch s1 x s2) = do
  r <- evalStmt st s1
  case r of
    EOk st' -> okE st'
    EErr v  -> evalStmt st { sVar = Map.insert x v (sVar st) } s2
-- (S-Print) — evaluate, print, leave state unchanged.
--   Strings print unquoted; other values use the same pretty-printer
--   the final state-dump uses.
evalStmt st (SPrint e) =
  evalExpr st e `bindE` \v -> do
    case v of
      VString s -> putStrLn s
      _         -> putStrLn (showVal v)
    okE st

------------------------------------------------------------------------
--  Pretty printing
------------------------------------------------------------------------

showVal :: Value -> String
showVal (VString s)  = show s
showVal (VNumber n)
  | fromInteger (floor n :: Integer) == n = show (floor n :: Integer)
  | otherwise                             = show n
showVal (VBool   b)  = if b then "true" else "false"
showVal (VRecord fs) =
  "{" ++ intercalate ", " [ f ++ " = " ++ showVal v | (f,v) <- fs ] ++ "}"

showArgs :: [Value] -> String
showArgs vs = "[" ++ intercalate ", " (map showVal vs) ++ "]"

truncate' :: Int -> String -> String
truncate' n s | length s <= n = s
              | otherwise     = take n s ++ "..."

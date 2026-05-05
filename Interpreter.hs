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
  , valueToJsonText
  ) where

import           Control.Exception          (SomeException, try)
import           Data.Char                  (isSpace, toLower)
import           Data.List                  (intercalate, isInfixOf)
import qualified Data.Map.Strict            as Map
import           Data.Map.Strict            (Map)
import qualified Data.Aeson                 as A
import qualified Data.Aeson.Key             as K
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
  | ADCustom  Value (Maybe String)        -- custom(prompt, m)
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
evalExpr st (EList es) = go es []
  where
    go []     acc = okE (VList (reverse acc))
    go (e:rs) acc = evalExpr st e `bindE` \v -> go rs (v:acc)
-- (E-Op)
evalExpr st (EBin op e1 e2) =
  evalExpr st e1 `bindE` \v1 ->
  evalExpr st e2 `bindE` \v2 ->
    case applyBinOp op v1 v2 of
      Just v  -> okE v
      Nothing -> errE (VString ("type error in operator " ++ show op))
-- (E-Agent-*) selected from the registered agent definition
evalExpr st (ECall name es)
  | isBuiltin name = evalArgs st es `bindE` runBuiltin name
  | otherwise = case Map.lookup name (sAgent st) of
      Nothing  -> errE (VString ("unknown agent: " ++ name))
      Just def -> evalArgs st es `bindE` \vs -> applyAgent st def vs

evalArgs :: EvalState -> [Expr] -> EvalIO [Value]
evalArgs _  []     = okE []
evalArgs st (e:es) =
  evalExpr st e  `bindE` \v  ->
  evalArgs st es `bindE` \vs -> okE (v:vs)

isBuiltin :: String -> Bool
isBuiltin name = name `elem` ["parse_json", "stringify", "to_json"]

runBuiltin :: String -> [Value] -> EvalIO Value
runBuiltin "parse_json" [VString s] =
  case parseJsonValue s of
    Right v  -> okE v
    Left err -> errE (VString ("parse_json failed: " ++ err))
runBuiltin "parse_json" [_] = errE (VString "parse_json expects a string")
runBuiltin "parse_json" _   = errE (VString "parse_json expects exactly one argument")
runBuiltin "stringify" [v] = okE (VString (valueToJsonText v))
runBuiltin "stringify" _   = errE (VString "stringify expects exactly one argument")
runBuiltin "to_json" [v] = okE (VString (valueToJsonText v))
runBuiltin "to_json" _   = errE (VString "to_json expects exactly one argument")
runBuiltin name _ = errE (VString ("unknown builtin: " ++ name))

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
applyAgent st (ADFixed   k)        vs  = runFixedWorkflow (sConfig st) k vs
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
--   otherwise fallback must be explicitly allowed in config.
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
  let prompt = intercalate "\n\n" (map llmPromptText vs)
      mkResponse txt = VRecord
        [ ("backend",  VString "llm")
        , ("model",    VString m)
        , ("prompt",   VString prompt)
        , ("response", VString txt)
        , ("output",   llmOutputValue txt)
        , ("raw_output", VString txt)
        ]
  llmAccess cfg `bindE` \mode -> case mode of
    LlmStub    -> okE (mkResponse ("(simulated " ++ m ++ " response)"))
    LlmLive key -> do
      r <- callClaude key m prompt
      case r of
        Right txt -> okE (mkResponse txt)
        Left err  -> errE (VString ("LLM call failed: " ++ err))

-- | (E-Agent-Custom) — optional real call when opted in via config.
runLLM :: ConfigEnv -> Maybe String -> Value -> Value -> EvalIO Value
runLLM cfg modelOverride prompt input = do
  let model = maybe (workflowModel cfg) id modelOverride
  putStrLn $ "[customAI " ++ model ++ "] prompt=" ++ showVal prompt
                              ++ " input=" ++ showVal input
  llmAccess cfg `bindE` \mode -> case mode of
    LlmStub -> okE (VRecord
      [ ("model",  VString model)
      , ("prompt", prompt)
      , ("input",  input)
      , ("output", VString ("(simulated " ++ model ++ " response)"))
      , ("raw_output", VString ("(simulated " ++ model ++ " response)"))
      ])
    LlmLive key -> do
      r <- callClaudeWithSystem key model (valueText prompt) (llmPromptText input)
      case r of
        Right txt -> okE (VRecord
          [ ("model",  VString model)
          , ("prompt", prompt)
          , ("input",  input)
          , ("output", llmOutputValue txt)
          , ("raw_output", VString txt)
          ])
        Left err  -> errE (VString ("CustomAI call failed: " ++ err))

runFixedWorkflow :: ConfigEnv -> Kind -> [Value] -> EvalIO Value
runFixedWorkflow _ Merger vs = okE (withWorkflowOutput Merger (runFixed Merger vs))
runFixedWorkflow cfg k vs =
  llmAccess cfg `bindE` \mode -> case mode of
    LlmStub     -> okE (withWorkflowOutput k (runFixed k vs))
    LlmLive key -> runFixedLLM cfg key k vs

runFixedLLM :: ConfigEnv -> String -> Kind -> [Value] -> EvalIO Value
runFixedLLM cfg apiKey k vs = do
  let model        = workflowModel cfg
      systemPrompt = workflowSystemPrompt k
      input        = workflowInput vs
      format       = workflowFormat k
  putStrLn $ "[agent  " ++ show k ++ " " ++ model ++ "] " ++ showArgs vs
  r <- callClaudeWithSystem apiKey model systemPrompt input
  case r of
    Right txt -> okE (VRecord
      [ ("kind",   VString (show k))
      , ("model",  VString model)
      , ("system", VString systemPrompt)
      , ("input",  VRecord (zip [ "arg" ++ show i | i <- [0 :: Int ..] ] vs))
      , ("format", VString format)
      , ("output", llmOutputValue txt)
      , ("raw_output", VString txt)
      ])
    Left err -> errE (VString ("FixedAgent " ++ show k ++ " call failed: " ++ err))

workflowModel :: ConfigEnv -> String
workflowModel cfg =
  case Map.lookup "llm_model" cfg of
    Just (VString m) -> m
    _ -> case Map.lookup "model" cfg of
      Just (VString m) -> m
      _                -> "claude-opus-4-7"

workflowSystemPrompt :: Kind -> String
workflowSystemPrompt k = intercalate "\n"
  [ workflowRole k
  , "You are one node in an AWL workflow graph, similar to an n8n node."
  , "Transform the input into the required schema so the next node can consume it."
  , "Return only valid JSON. Do not wrap the JSON in markdown."
  , "Schema: " ++ workflowFormat k
  ]

workflowRole :: Kind -> String
workflowRole Planner             = "You are a planning agent. Turn a goal into a concrete execution plan."
workflowRole TaskSplitter        = "You are a task splitting agent. Break work into small ordered tasks."
workflowRole Extractor           = "You are an extraction agent. Pull out the key structured facts from the input."
workflowRole Critic              = "You are a critic agent. Evaluate quality, identify weaknesses, and score the input."
workflowRole Writer              = "You are a writing agent. Produce a polished draft from the input."
workflowRole Summarizer          = "You are a summarization agent. Produce a concise summary of the input."
workflowRole Validator           = "You are a validation agent. Decide whether the input is complete, usable, and schema-compliant."
workflowRole Guardrail           = "You are a guardrail agent. Check safety, privacy, and policy risks."
workflowRole Router              = "You are a router agent. Choose the best next workflow route."
workflowRole Merger              = "You are a merge agent. Combine multiple upstream inputs into one coherent object."
workflowRole Ranker              = "You are a ranking agent. Rank multiple options by usefulness."

workflowFormat :: Kind -> String
workflowFormat Planner             = "{\"goal\":\"...\",\"steps\":[\"...\"],\"dependencies\":[\"...\"],\"next_input\":\"...\"}"
workflowFormat TaskSplitter        = "{\"tasks\":[{\"id\":1,\"task\":\"...\",\"input\":\"...\"}],\"next_input\":\"...\"}"
workflowFormat Extractor           = "{\"facts\":[\"...\"],\"entities\":[\"...\"],\"next_input\":\"...\"}"
workflowFormat Critic              = "{\"score\":0.0,\"issues\":[\"...\"],\"recommendations\":[\"...\"],\"next_input\":\"...\"}"
workflowFormat Writer              = "{\"draft\":\"...\",\"next_input\":\"...\"}"
workflowFormat Summarizer          = "{\"summary\":\"...\",\"key_points\":[\"...\"],\"next_input\":\"...\"}"
workflowFormat Validator           = "{\"valid\":true,\"issues\":[\"...\"],\"fixed_input\":\"...\",\"next_input\":\"...\"}"
workflowFormat Guardrail           = "{\"safe\":true,\"risks\":[\"...\"],\"redacted_input\":\"...\",\"next_input\":\"...\"}"
workflowFormat Router              = "{\"route\":\"...\",\"reason\":\"...\",\"next_input\":\"...\"}"
workflowFormat Merger              = "{\"merged\":\"...\",\"sources_used\":[\"...\"],\"next_input\":\"...\"}"
workflowFormat Ranker              = "{\"ranked\":[{\"rank\":1,\"item\":\"...\",\"reason\":\"...\"}],\"next_input\":\"...\"}"

workflowInput :: [Value] -> String
workflowInput [] = "No inputs."
workflowInput vs = intercalate "\n"
  [ "arg" ++ show i ++ ": " ++ llmPromptText v
  | (i, v) <- zip [0 :: Int ..] vs
  ]

withWorkflowOutput :: Kind -> Value -> Value
withWorkflowOutput k v@(VRecord fs)
  | any ((== "output") . fst) fs = v
  | otherwise                    = VRecord (fs ++ [("output", workflowDefaultOutput k v)])
withWorkflowOutput _ v = VRecord [("value", v), ("output", v)]

workflowDefaultOutput :: Kind -> Value -> Value
workflowDefaultOutput Planner             = id
workflowDefaultOutput TaskSplitter        = fieldOrSelf "tasks"
workflowDefaultOutput Extractor           = fieldOrSelf "extracted"
workflowDefaultOutput Critic              = fieldOrSelf "critique"
workflowDefaultOutput Writer              = fieldOrSelf "draft"
workflowDefaultOutput Summarizer          = fieldOrSelf "summary"
workflowDefaultOutput Validator           = fieldOrSelf "payload"
workflowDefaultOutput Guardrail           = fieldOrSelf "payload"
workflowDefaultOutput Router              = fieldOrSelf "route"
workflowDefaultOutput Merger              = fieldOrSelf "merged"
workflowDefaultOutput Ranker              = fieldOrSelf "ranked"

fieldOrSelf :: String -> Value -> Value
fieldOrSelf f v@(VRecord fs) = maybe v id (lookup f fs)
fieldOrSelf _ v              = v

data LlmAccess = LlmStub | LlmLive String

llmAccess :: ConfigEnv -> EvalIO LlmAccess
llmAccess cfg = case Map.lookup "real_llm" cfg of
  Just (VBool True) -> do
    mKey <- lookupEnv "ANTHROPIC_API_KEY"
    case mKey of
      Just key | not (null key) -> okE (LlmLive key)
      _ | llmStubAllowed cfg -> do
            putStrLn "[warn] real_llm = true but ANTHROPIC_API_KEY is not set; using deterministic stubs"
            okE LlmStub
        | otherwise -> errE (VString "real_llm is true but ANTHROPIC_API_KEY is not set; export ANTHROPIC_API_KEY or set allow_llm_fallback = true")
  _ -> okE LlmStub

llmStubAllowed :: ConfigEnv -> Bool
llmStubAllowed cfg =
  Map.lookup "allow_llm_fallback" cfg == Just (VBool True) ||
  Map.lookup "allow_stub_fallback" cfg == Just (VBool True)

-- | Single-shot Claude Messages API call. Raw HTTP — no Haskell SDK exists.
--   See https://docs.claude.com/en/api/messages
callClaude :: String -> String -> String -> IO (Either String String)
callClaude apiKey model prompt = callClaudeWithSystem apiKey model "" prompt

callClaudeWithSystem :: String -> String -> String -> String -> IO (Either String String)
callClaudeWithSystem apiKey model systemPrompt prompt = do
  let body = A.object
        ([ "model"      A..= model
         , "max_tokens" A..= (1024 :: Int)
         ]
        ++ [ "system" A..= systemPrompt | not (null systemPrompt) ]
        ++ [ "messages"   A..=
              [ A.object
                  [ "role"    A..= ("user" :: T.Text)
                  , "content" A..= prompt
                  ]
              ]
           ])
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

llmOutputValue :: String -> Value
llmOutputValue txt =
  case parseJsonValue txt of
    Right v -> v
    Left _  -> VString txt

parseJsonValue :: String -> Either String Value
parseJsonValue txt = jsonToValue <$> A.eitherDecode (LBS.pack txt)

jsonToValue :: A.Value -> Value
jsonToValue (A.String t) = VString (T.unpack t)
jsonToValue (A.Number n) = VNumber (realToFrac n)
jsonToValue (A.Bool b)   = VBool b
jsonToValue A.Null       = VNull
jsonToValue (A.Array a)  = VList (map jsonToValue (V.toList a))
jsonToValue (A.Object o) =
  VRecord [ (K.toString k, jsonToValue v) | (k, v) <- KM.toList o ]

valueToJsonText :: Value -> String
valueToJsonText = LBS.unpack . A.encode . valueToJson

valueToJson :: Value -> A.Value
valueToJson (VString s)  = A.String (T.pack s)
valueToJson (VNumber n)  = A.toJSON n
valueToJson (VBool b)    = A.Bool b
valueToJson (VList vs)   = A.Array (V.fromList (map valueToJson vs))
valueToJson (VRecord fs) =
  A.Object (KM.fromList [ (K.fromString k, valueToJson v) | (k, v) <- fs ])
valueToJson VNull        = A.Null

synthRecord :: String -> String -> [Value] -> Value
synthRecord backend target vs = VRecord
  [ ("backend", VString backend)
  , ("target",  VString target)
  , ("args",    VRecord (zip [ "arg" ++ show i | i <- [0 :: Int ..] ] vs))
  , ("output",  VRecord (zip [ "arg" ++ show i | i <- [0 :: Int ..] ] vs))
  ]

-- | Stubs for the 11 fixed agent kinds.  They do not call any model;
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

llmPromptText :: Value -> String
llmPromptText (VString s) = s
llmPromptText v           = valueToJsonText v

trimStr :: String -> String
trimStr = f . f where f = dropWhile isSpace . reverse

lowerStr :: String -> String
lowerStr = map toLower

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
-- Extract first 5 words of the source.
runFixedOne Extractor v =
  let txt   = valueText v
      first = take 5 (words txt)
  in VRecord
       [ ("source",     v)
       , ("extracted",  VString (unwords first))
       , ("word_count", VNumber (fromIntegral (length first)))
       ]
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
runFixedOne Writer     v = VRecord [("draft",   VString ("draft about: " ++ showVal v))]
runFixedOne Summarizer v = VRecord [("summary", VString (truncate' 80 (showVal v)))]
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
showVal (VList vs)   = "[" ++ intercalate ", " (map showVal vs) ++ "]"
showVal (VRecord fs) =
  "{" ++ intercalate ", " [ f ++ " = " ++ showVal v | (f,v) <- fs ] ++ "}"
showVal VNull        = "null"

showArgs :: [Value] -> String
showArgs vs = "[" ++ intercalate ", " (map showVal vs) ++ "]"

truncate' :: Int -> String -> String
truncate' n s | length s <= n = s
              | otherwise     = take n s ++ "..."

{-# LANGUAGE FlexibleContexts #-}

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

import           Data.List       (intercalate)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

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
--   The python/http/llm dispatchers are stubs: rather than make a
--   network call we synthesise a record describing the call so the
--   semantics are observable in tests.  Replace these with real
--   integrations in production.
runBackend :: ConfigEnv -> Backend -> [Value] -> EvalIO Value
runBackend _ (BMock v)   _  = okE v
runBackend _ (BPython f) vs = do
  putStrLn $ "[python " ++ f ++ "] " ++ showArgs vs
  okE (synthRecord "python" f vs)
runBackend _ (BHttp u) vs = do
  putStrLn $ "[http   " ++ u ++ "] " ++ showArgs vs
  okE (synthRecord "http" u vs)
runBackend _ (BLlm m) vs = do
  putStrLn $ "[llm    " ++ m ++ "] " ++ showArgs vs
  okE (synthRecord "llm" m vs)

-- | Stub for @llm_call@ in (E-Agent-Custom).
runLLM :: ConfigEnv -> String -> Value -> Value -> EvalIO Value
runLLM _ model prompt input = do
  putStrLn $ "[customAI " ++ model ++ "] prompt=" ++ showVal prompt
                              ++ " input=" ++ showVal input
  okE (VRecord
    [ ("model",  VString model)
    , ("prompt", prompt)
    , ("input",  input)
    , ("output", VString ("(simulated " ++ model ++ " response)"))
    ])

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

runFixedOne :: Kind -> Value -> Value
runFixedOne Planner v = VRecord
  [ ("goal",  v)
  , ("steps", VRecord [("0", VString "analyze")
                      ,("1", VString "act")
                      ,("2", VString "review")])
  ]
runFixedOne TaskSplitter        v = VRecord [("tasks", VRecord [("0", v), ("1", v)])]
runFixedOne Searcher            v = VRecord [("query", v), ("hits", VString "[search hits]")]
runFixedOne Extractor           v = VRecord [("source", v), ("extracted", v)]
runFixedOne Cleaner             v = VRecord [("cleaned", v)]
runFixedOne Deduplicator        v = VRecord [("deduped", v)]
runFixedOne Formatter           v = VRecord [("formatted", VString (showVal v))]
runFixedOne Critic              v = VRecord [("input", v)
                                             ,("critique", VString "looks ok")
                                             ,("score", VNumber 0.85)]
runFixedOne FactChecker         v = VRecord [("input", v), ("verified", VBool True)]
runFixedOne ConfidenceEstimator v = VRecord [("input", v), ("confidence", VNumber 0.9)]
runFixedOne Writer              v = VRecord [("draft", VString ("draft about: " ++ showVal v))]
runFixedOne Summarizer          v = VRecord [("summary", VString (truncate' 80 (showVal v)))]
runFixedOne Rewriter            v = VRecord [("rewritten", v)]
runFixedOne Validator           v = VRecord [("valid", VBool True), ("payload", v)]
runFixedOne Guardrail           v = VRecord [("safe",  VBool True), ("payload", v)]
runFixedOne Fallback            v = VRecord [("fallback", v)]
runFixedOne Router              v = VRecord [("route", VString "default"), ("payload", v)]
runFixedOne Merger              v = VRecord [("merged", v)]
runFixedOne Ranker              v = VRecord [("ranked", v)]

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

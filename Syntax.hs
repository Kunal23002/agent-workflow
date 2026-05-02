-- | Abstract syntax of the Agent Workflow Language (section 1 of the spec).
--
-- This module is intentionally pure: no Parsec, no IO.  Both the parser
-- ('Parser') and the big-step interpreter ('Interpreter') depend on it.
module Syntax
  ( Value(..)
  , BinOp(..)
  , Expr(..)
  , Backend(..)
  , Kind(..)
  , Stmt(..)
  ) where

-- | Runtime values  (v ::= c | { f = v, ... })
--   The spec also has @err(v)@ as a value; the interpreter represents it
--   instead as an evaluation outcome so it cannot appear inside records.
data Value
  = VString String
  | VNumber Double
  | VBool   Bool
  | VRecord [(String, Value)]
  deriving (Eq, Show)

-- | Binary operators: e₁ ⊕ e₂  (>, <, ==, +, …)
data BinOp
  = OpAdd | OpSub | OpMul | OpDiv
  | OpEq  | OpNeq | OpGt  | OpLt | OpGte | OpLte
  | OpAnd | OpOr
  deriving (Eq, Show)

-- | Expressions  (e ::= …)
data Expr
  = EConst  Value                       -- c
  | EVar    String                      -- x
  | EProj   Expr String                 -- e.f
  | ECall   String [Expr]               -- A(e₁, …, eₙ)
  | EBin    BinOp Expr Expr             -- e₁ ⊕ e₂
  | ERecord [(String, Expr)]            -- { fᵢ = eᵢ }
  deriving (Eq, Show)

-- | Backends supplied to @agent A from b@.
data Backend
  = BPython String                      -- python:"module.function"
  | BHttp   String                      -- http:"url"
  | BLlm    String                      -- llm:"model-id"
  | BMock   Value                       -- mock:"value"
  deriving (Eq, Show)

-- | Fixed agent kinds  (k ::= Planner | TaskSplitter | …)
data Kind
  = Planner | TaskSplitter | Searcher | Extractor | Cleaner
  | Deduplicator | Formatter | Critic | FactChecker
  | ConfidenceEstimator | Writer | Summarizer | Rewriter
  | Validator | Guardrail | Fallback | Router | Merger | Ranker
  deriving (Eq, Show, Bounded, Enum)

-- | Statements  (s ::= …)
data Stmt
  = SConfig       [(String, Expr)]              -- config { c = e, … }
  | SAgentBackend String Backend                 -- agent A from b
  | SAgentFixed   String Kind                    -- agent A = FixedAgent(k)
  | SAgentCustom  String Expr String             -- agent A = CustomAI(prompt = e, model = m)
  | SLet          String Expr                    -- let x = e
  | SIf           Expr Stmt Stmt                 -- if e then s₁ else s₂
  | SSeq          Stmt Stmt                      -- s₁ ; s₂
  | SFail         Expr                           -- fail e
  | SRetry        Int  Stmt                      -- retry n s
  | STryCatch     Stmt String Stmt               -- try s₁ catch x => s₂
  deriving (Eq, Show)

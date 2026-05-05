-- | Concrete syntax → abstract syntax for the Agent Workflow Language
--   (section 2 of the spec).  Built on Parsec.
module Parser
  ( parseProgram
  ) where

import           Control.Monad        (void)
import qualified Text.Parsec          as P
import qualified Text.Parsec.Expr     as PE
import qualified Text.Parsec.Token    as PT
import           Text.Parsec.Language (emptyDef)
import           Text.Parsec.String   (Parser)

import           Syntax

reservedNamesList :: [String]
reservedNamesList =
  [ "config", "agent", "from", "let", "if", "then", "else"
  , "fail", "retry", "try", "catch", "print"
  , "FixedAgent", "CustomAI", "prompt", "model"
  , "true", "false", "null"
  , "python", "http", "llm", "mock"
  ]

-- | The lexical structure of the language.
langDef :: PT.LanguageDef ()
langDef = emptyDef
  { PT.commentLine     = "//"
  , PT.commentStart    = "/*"
  , PT.commentEnd      = "*/"
  , PT.identStart      = P.letter P.<|> P.char '_'
  , PT.identLetter     = P.alphaNum P.<|> P.char '_'
  , PT.reservedOpNames =
      [ "=", "==", "!=", ">", "<", ">=", "<="
      , "+", "-", "*", "/", "&&", "||", ".", "=>", ":"
      ]
  , PT.reservedNames   = reservedNamesList
  , PT.caseSensitive   = True
  }

lexer :: PT.TokenParser ()
lexer = PT.makeTokenParser langDef

identifier     :: Parser String
identifier     = PT.identifier     lexer
fieldName      :: Parser String
fieldName      = identifier P.<|> P.choice (map reservedName reservedNamesList)
  where
    reservedName name = P.try (reserved name >> return name)
reserved       :: String -> Parser ()
reserved       = PT.reserved       lexer
reservedOp     :: String -> Parser ()
reservedOp     = PT.reservedOp     lexer
parens         :: Parser a -> Parser a
parens         = PT.parens         lexer
semi           :: Parser String
semi           = PT.semi           lexer
comma          :: Parser String
comma          = PT.comma          lexer
commaSep       :: Parser a -> Parser [a]
commaSep       = PT.commaSep       lexer
whiteSpace     :: Parser ()
whiteSpace     = PT.whiteSpace     lexer
stringLit      :: Parser String
stringLit      = PT.stringLiteral  lexer
natural        :: Parser Integer
natural        = PT.natural        lexer
naturalOrFloat :: Parser (Either Integer Double)
naturalOrFloat = PT.naturalOrFloat lexer
symbol         :: String -> Parser String
symbol         = PT.symbol         lexer

-- ---------- Expressions ----------------------------------------------

expr :: Parser Expr
expr = opExpr P.<?> "expression"

-- | Standard precedence climber for binary operators.
opExpr :: Parser Expr
opExpr = PE.buildExpressionParser table postfix
  where
    binL s op = PE.Infix (reservedOp s >> return (EBin op)) PE.AssocLeft
    binN s op = PE.Infix (reservedOp s >> return (EBin op)) PE.AssocNone
    table =
      [ [ binL "*"  OpMul, binL "/"  OpDiv ]
      , [ binL "+"  OpAdd, binL "-"  OpSub ]
      , [ binN "==" OpEq , binN "!=" OpNeq
        , binN ">=" OpGte, binN "<=" OpLte
        , binN ">"  OpGt , binN "<"  OpLt  ]
      , [ binL "&&" OpAnd ]
      , [ binL "||" OpOr  ]
      ]

-- | After an atom we may chain field projections @.f@.
postfix :: Parser Expr
postfix = atom >>= go
  where
    go e = (do reservedOp "."; f <- fieldName; go (EProj e f))
           P.<|> return e

atom :: Parser Expr
atom =
        parens expr
  P.<|> listLit
  P.<|> recordLit
  P.<|> stringLitE
  P.<|> numLitE
  P.<|> nullLitE
  P.<|> boolLitE
  P.<|> identAtom

stringLitE :: Parser Expr
stringLitE = (EConst . VString) <$> stringLit

numLitE :: Parser Expr
numLitE = do
  n <- naturalOrFloat
  return $ EConst $ VNumber $ case n of
    Left  i -> fromIntegral i
    Right f -> f

boolLitE :: Parser Expr
boolLitE = (reserved "true"  >> return (EConst (VBool True)))
     P.<|> (reserved "false" >> return (EConst (VBool False)))

nullLitE :: Parser Expr
nullLitE = reserved "null" >> return (EConst VNull)

listLit :: Parser Expr
listLit = do
  void (symbol "[")
  es <- commaSep expr
  void (symbol "]")
  return (EList es)

recordLit :: Parser Expr
recordLit = do
  void (symbol "{")
  fs <- commaSep $ do
    f <- fieldName
    reservedOp "="
    e <- expr
    return (f, e)
  void (symbol "}")
  return (ERecord fs)

-- | An identifier optionally followed by an argument list
--   becomes either an agent call @A(e,…)@ or a variable @x@.
identAtom :: Parser Expr
identAtom = do
  name  <- identifier
  margs <- P.optionMaybe (parens (commaSep expr))
  return $ case margs of
    Just args -> ECall name args
    Nothing   -> EVar  name

-- ---------- Statements ----------------------------------------------

-- | A program is one or more statements separated (and optionally
--   terminated) by @;@.
program :: Parser Stmt
program = do
  whiteSpace
  ss <- P.sepEndBy1 stmt semi
  P.eof
  return (foldr1 SSeq ss)

stmt :: Parser Stmt
stmt = P.choice
  [ stmtBlock
  , stmtConfig
  , stmtAgent
  , stmtLet
  , stmtIf
  , stmtFail
  , stmtRetry
  , stmtTryCatch
  , stmtPrint
  ] P.<?> "statement"

-- | @{ s₁ ; s₂ ; … }@ groups statements; resolved to right-nested
--   'SSeq' so it satisfies S-Seq.
stmtBlock :: Parser Stmt
stmtBlock = do
  void (symbol "{")
  ss <- P.sepEndBy1 stmt semi
  void (symbol "}")
  return (foldr1 SSeq ss)

stmtConfig :: Parser Stmt
stmtConfig = do
  reserved "config"
  void (symbol "{")
  fs <- commaSep $ do
    c <- fieldName
    reservedOp "="
    e <- expr
    return (c, e)
  void (symbol "}")
  return (SConfig fs)

stmtAgent :: Parser Stmt
stmtAgent = do
  reserved "agent"
  name <- identifier
  P.choice
    [ do reserved "from"; b <- backendP; return (SAgentBackend name b)
    , do reservedOp "=" ; agentRhs name
    ]

agentRhs :: String -> Parser Stmt
agentRhs name =
       (do reserved "FixedAgent"; k <- parens kindP; return (SAgentFixed name k))
  P.<|>(do reserved "CustomAI"
           void (symbol "(")
           reserved "prompt"; reservedOp "="; pe <- expr
           m <- P.optionMaybe (do
             void comma
             reserved "model" ; reservedOp "="; stringLit)
           void (symbol ")")
           return (SAgentCustom name pe m))

backendP :: Parser Backend
backendP = P.choice
  [ reserved "python" >> reservedOp ":" >> BPython <$> stringLit
  , reserved "http"   >> reservedOp ":" >> BHttp   <$> stringLit
  , reserved "llm"    >> reservedOp ":" >> BLlm    <$> stringLit
  , reserved "mock"   >> reservedOp ":" >> (BMock . VString) <$> stringLit
  ] P.<?> "backend"

kindP :: Parser Kind
kindP = do
  k <- identifier
  case k of
    "Planner"             -> return Planner
    "TaskSplitter"        -> return TaskSplitter
    "Extractor"           -> return Extractor
    "Critic"              -> return Critic
    "Writer"              -> return Writer
    "Summarizer"          -> return Summarizer
    "Validator"           -> return Validator
    "Guardrail"           -> return Guardrail
    "Router"              -> return Router
    "Merger"              -> return Merger
    "Ranker"              -> return Ranker
    _                     -> P.unexpected ("agent kind " ++ show k)

stmtLet :: Parser Stmt
stmtLet = do
  reserved "let"
  x <- identifier
  reservedOp "="
  e <- expr
  return (SLet x e)

stmtIf :: Parser Stmt
stmtIf = do
  reserved "if";   c  <- expr
  reserved "then"; s1 <- stmt
  reserved "else"; s2 <- stmt
  return (SIf c s1 s2)

stmtFail :: Parser Stmt
stmtFail = reserved "fail" >> SFail <$> expr

stmtRetry :: Parser Stmt
stmtRetry = do
  reserved "retry"
  n <- natural
  s <- stmt
  return (SRetry (fromIntegral n) s)

stmtTryCatch :: Parser Stmt
stmtTryCatch = do
  reserved "try"  ; s1 <- stmt
  reserved "catch"; x  <- identifier
  reservedOp "=>" ; s2 <- stmt
  return (STryCatch s1 x s2)

stmtPrint :: Parser Stmt
stmtPrint = reserved "print" >> SPrint <$> expr

-- | Top-level entry point.
parseProgram :: FilePath -> String -> Either P.ParseError Stmt
parseProgram = P.parse program

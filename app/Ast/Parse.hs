{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Ast.Parse (program) where

import Ast.Types
import Control.Applicative (Alternative (many, (<|>)), optional)
import Data.Functor (($>))
import Text.ParserCombinators.Parsec qualified as PC
import TypeSystem
import Prelude hiding (exp, id, lex)

comment :: PC.Parser ()
comment =
  PC.string "//"
    *> many (PC.satisfy (/= '\n'))
    *> ( PC.try (PC.char '\n')
           $> ()
           <|> PC.eof
       )

-- Space consumer: skips whitespace and comments
sc :: PC.Parser ()
sc = PC.skipMany (PC.try comment <|> PC.space $> ())

lex :: PC.Parser a -> PC.Parser a
lex p = p <* sc

keyword :: String -> PC.Parser String
keyword kw = lex (PC.string kw)

ifkw :: PC.Parser String
ifkw = keyword "if"

whilekw :: PC.Parser String
whilekw = keyword "while"

elsekw :: PC.Parser String
elsekw = keyword "else"

returnkw :: PC.Parser String
returnkw = keyword "return"

symbol :: String -> PC.Parser String
symbol sym = lex (PC.string sym)

parens :: PC.Parser a -> PC.Parser a
parens p = symbol "(" *> p <* symbol ")"

brackets :: PC.Parser a -> PC.Parser a
brackets p = symbol "[" *> p <* symbol "]"

braces :: PC.Parser a -> PC.Parser a
braces p = symbol "{" *> p <* symbol "}"

commaSep :: PC.Parser a -> PC.Parser [a]
commaSep p = PC.sepBy p (symbol ",")

id :: PC.Parser Id
id =
  let isFirstChar c = c `elem` ['a' .. 'z'] ++ ['A' .. 'Z'] ++ "_"
      isOtherChar c = isFirstChar c || c `elem` ['0' .. '9']
   in lex (liftA2 (:) (PC.satisfy isFirstChar) (many (PC.satisfy isOtherChar)))

ty :: PC.Parser Ty
ty =
  let parseInt = (keyword "int" $> IntTy)
      parseChar = (keyword "char" $> CharTy)
      parseBool = (keyword "bool" $> BoolTy)
      parseVoid = (keyword "void" $> VoidTy)
   in lex (parseInt <|> parseChar <|> parseBool <|> parseVoid)

num :: PC.Parser Int
num =
  let isDigit c = c `elem` ['0' .. '9']
   in read <$> lex (PC.many1 (PC.satisfy isDigit))

char :: PC.Parser Char
char = lex $ do
  _ <- PC.char '\''
  c <- PC.try (PC.char '\\' *> escapeChar) <|> PC.satisfy validChar
  _ <- PC.char '\''
  return c
  where
    validChar c = c /= '\'' && c /= '\\' && c >= ' ' && c <= '~'
    escapeChar =
      PC.choice
        [ PC.char '\\' $> '\\',
          PC.char '\'' $> '\'',
          PC.char '\"' $> '\"',
          PC.char 'n' $> '\n',
          PC.char 'r' $> '\r',
          PC.char 't' $> '\t'
        ]

exp :: PC.Parser RawExp
exp = eqexp
  where
    factor =
      PC.try callexp
        <|> PC.try arraccess
        <|> PC.try idexp
        <|> PC.try parenexp
        <|> PC.try charexp
        <|> numexp
      where
        numexp = do
          numberValue <- num
          return Exp {expAnnot = (), expInner = NumberExp {numberValue}}
        charexp = do
          charValue <- char
          return Exp {expAnnot = (), expInner = CharExp {charValue}}
        idexp = do
          idName <- id
          return Exp {expAnnot = (), expInner = IdExp {idName}}
        parenexp = parens exp
        callexp = do
          callId <- id
          callArgs <- parens (commaSep exp)
          return Exp {expAnnot = (), expInner = Call {callId, callArgs}}
        arraccess = do
          arrId <- id
          arrIndex <- brackets exp
          return Exp {expAnnot = (), expInner = ArrAccess {arrId, arrIndex}}

    unaryexp = do
      op <- optional $ PC.try (symbol "-" $> UnarySub) <|> (symbol "!" $> UnaryNot)
      case op of
        Just unaryOp -> do
          e <- PC.try unaryexp <|> factor
          return Exp {expAnnot = (), expInner = UnaryExp {unaryOp, unaryExp = e}}
        Nothing -> factor

    mulexp = do
      binLeft <- unaryexp
      maybeOp <-
        optional
          ( PC.try (symbol "*") $> Mul
              <|> PC.try (symbol "/" $> Div)
              <|> symbol "%" $> Mod
          )
      case maybeOp of
        Just binOp -> do
          binRight <- mulexp
          return Exp {expAnnot = (), expInner = BinExp {binLeft, binOp, binRight}}
        Nothing -> return binLeft

    addsubexp = do
      binLeft <- mulexp
      maybeOp <-
        optional
          ( PC.try
              (symbol "+" $> Add)
              <|> symbol "-" $> Sub
          )
      case maybeOp of
        Just binOp -> do
          binRight <- addsubexp
          return Exp {expAnnot = (), expInner = BinExp {binLeft, binOp, binRight}}
        Nothing -> return binLeft

    eqexp = do
      binLeft <- addsubexp
      maybeOp <-
        optional
          ( PC.try (symbol "==" $> Equal)
              <|> PC.try (symbol "!=" $> NotEqual)
              <|> PC.try (symbol "<=" $> LessThanOrEqual)
              <|> PC.try (symbol ">=" $> GreaterThanOrEqual)
              <|> PC.try (symbol "<" $> LessThan)
              <|> symbol ">" $> GreaterThan
          )
      case maybeOp of
        Just binOp -> do
          binRight <- eqexp
          return Exp {expAnnot = (), expInner = BinExp {binLeft, binOp, binRight}}
        Nothing -> return binLeft

vardef :: PC.Parser VarDef
vardef = do
  varDefTy <- ty
  varDefId <- id
  return VarDef {varDefId, varDefTy}

stmt :: PC.Parser RawStmt
stmt =
  PC.try letstmt
    <|> PC.try letarrstmt
    <|> PC.try retstmt
    <|> PC.try ifstmt
    <|> PC.try whilestmt
    <|> PC.try assignstmt
    <|> PC.try assignarrstmt
    <|> expstmt
  where
    letstmt = do
      letVarDef <- vardef
      _ <- symbol "="
      letExp <- exp
      _ <- symbol ";"
      return $ LetStmt {letVarDef, letExp}

    letarrstmt = do
      letArrVarDef <- vardef
      letArrSize <- brackets num
      _ <- symbol "="
      letArrElems <- braces (commaSep exp)
      _ <- symbol ";"
      return $ LetArrStmt {letArrVarDef, letArrSize, letArrElems}

    assignstmt = do
      assignId <- id
      _ <- symbol "="
      assignExp <- exp
      _ <- symbol ";"
      return $ AssignStmt {assignId, assignExp}

    assignarrstmt = do
      assignArrId <- id
      assignArrIndex <- brackets exp
      _ <- symbol "="
      assignArrExp <- exp
      _ <- symbol ";"
      return $ AssignArrStmt {assignArrId, assignArrIndex, assignArrExp}

    retstmt = do
      _ <- returnkw
      returnExp <- optional exp
      _ <- symbol ";"
      return $ ReturnStmt {returnExp}

    expstmt = do
      stmtExp <- exp
      _ <- symbol ";"
      return ExpStmt {stmtExp}

    ifstmt = do
      _ <- ifkw
      ifCond <- parens exp
      ifBody <- block
      ifElseBody <- optional (elsekw *> block)
      return $ IfStmt {ifCond, ifBody, ifElseBody}

    whilestmt = do
      _ <- whilekw
      whileCond <- parens exp
      whileBody <- block
      return WhileStmt {whileCond, whileBody}

block :: PC.Parser RawBlock
block = do
  blockStmts <- braces (many stmt)
  return $ Block {blockAnnot = (), blockStmts}

fun :: PC.Parser RawFun
fun = do
  funRetTy <- ty
  funId <- id
  funArgs <- parens (commaSep vardef)
  funBody <- block
  return Fun {funId, funArgs, funRetTy, funBody}

externfun :: PC.Parser RawExternFun
externfun = do
  _ <- keyword "extern"
  externFunRetTy <- ty
  externFunId <- id
  externFunArgs <- parens (commaSep vardef)
  _ <- symbol ";"
  return
    ExternFun
      { externFunId,
        externFunArgs = varDefTy <$> externFunArgs,
        externFunRetTy
      }

program :: PC.Parser RawProgram
program = do
  _ <- sc
  defs <- many (PC.try (Left <$> fun) <|> (Right <$> externfun))
  _ <- PC.eof

  let initialProgram = Program {programAnnot = (), programFuncs = [], programExternFuns = [], programMainFun = Nothing}
  let programResult =
        foldl
          ( \acc f -> case f of
              Left fun ->
                case funId fun of
                  "main" -> acc {programMainFun = Just fun}
                  _ -> acc {programFuncs = programFuncs acc ++ [fun]}
              Right externFun -> acc {programExternFuns = programExternFuns acc ++ [externFun]}
          )
          initialProgram
          defs

  return programResult

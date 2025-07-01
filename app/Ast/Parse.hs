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
        [ PC.char '\\' $> '\\'
        , PC.char '\'' $> '\''
        , PC.char '\"' $> '\"'
        , PC.char 'n' $> '\n'
        , PC.char 'r' $> '\r'
        , PC.char 't' $> '\t'
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
          num <- num
          return Exp {annot = (), exp = NumberExp {num}}
        charexp = do
          char <- char
          return Exp {annot = (), exp = CharExp {char}}
        idexp = do
          id <- id
          return Exp {annot = (), exp = IdExp {id}}
        parenexp = parens exp
        callexp = do
          id <- id
          args <- parens (commaSep exp)
          return Exp {annot = (), exp = Call {id, args}}
        arraccess = do
          id <- id
          index <- brackets exp
          return Exp {annot = (), exp = ArrAccess {id, index}}

    unaryexp = do
      op <- optional $ PC.try (symbol "-" $> UnarySub) <|> (symbol "!" $> UnaryNot)
      case op of
        Just unop -> do
          e <- PC.try unaryexp <|> factor
          return Exp {annot = (), exp = UnaryExp {unop, exp = e}}
        Nothing -> factor

    mulexp = do
      left <- unaryexp
      maybeOp <-
        optional
          ( PC.try (symbol "*") $> Mul
              <|> PC.try (symbol "/" $> Div)
              <|> symbol "%" $> Mod
          )
      case maybeOp of
        Just op -> do
          right <- mulexp
          return Exp {annot = (), exp = BinExp {left, op, right}}
        Nothing -> return left

    addsubexp = do
      left <- mulexp
      maybeOp <-
        optional
          ( PC.try
              (symbol "+" $> Add)
              <|> symbol "-" $> Sub
          )
      case maybeOp of
        Just op -> do
          right <- addsubexp
          return Exp {annot = (), exp = BinExp {left, op, right}}
        Nothing -> return left

    eqexp = do
      left <- addsubexp
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
        Just op -> do
          right <- eqexp
          return Exp {annot = (), exp = BinExp {left, op, right}}
        Nothing -> return left

vardef :: PC.Parser VarDef
vardef = do
  ty <- ty
  id <- id
  return VarDef {id, ty}

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
      vardef <- vardef
      _ <- symbol "="
      exp <- exp
      _ <- symbol ";"
      return $ LetStmt {vardef, exp}

    letarrstmt = do
      vardef <- vardef
      size <- brackets num
      _ <- symbol "="
      elems <- braces (commaSep exp)
      _ <- symbol ";"
      return $ LetArrStmt {vardef, size, elems}

    assignstmt = do
      id <- id
      _ <- symbol "="
      exp <- exp
      _ <- symbol ";"
      return $ AssignStmt {id, exp}

    assignarrstmt = do
      id <- id
      index <- brackets exp
      _ <- symbol "="
      exp <- exp
      _ <- symbol ";"
      return $ AssignArrStmt {id, index, exp}

    retstmt = do
      _ <- returnkw
      retexp <- optional exp
      _ <- symbol ";"
      return $ ReturnStmt {retexp}

    expstmt = do
      exp <- exp
      _ <- symbol ";"
      return ExpStmt {exp}

    ifstmt = do
      _ <- ifkw
      cond <- parens exp
      ifBody <- block
      elseBody <- optional (elsekw *> block)
      return $ IfStmt {cond, ifBody, elseBody}

    whilestmt = do
      _ <- whilekw
      cond <- parens exp
      body <- block
      return WhileStmt {cond, body}

block :: PC.Parser RawBlock
block = do
  stmts <- braces (many stmt)
  return $ Block {annot = (), stmts}

fun :: PC.Parser RawFun
fun = do
  retty <- ty
  id <- id
  args <- parens (commaSep vardef)
  body <- block
  return Fun {id, args, retty, body}

externfun :: PC.Parser RawExternFun
externfun = do
  _ <- keyword "extern"
  retty <- ty
  id <- id
  args <- parens (commaSep vardef)
  _ <- symbol ";"
  return
    ExternFun
      { id,
        args = (.ty) <$> args,
        retty
      }

program :: PC.Parser RawProgram
program = do
  _ <- sc
  defs <- many (PC.try (Left <$> fun) <|> (Right <$> externfun))
  _ <- PC.eof

  let initialProgram = Program {annot = (), funcs = [], externFuns = [], mainFun = Nothing}
  let programResult =
        foldl
          ( \acc f -> case f of
              Left fun ->
                case fun.id of
                  "main" -> acc {mainFun = Just fun}
                  _ -> acc {funcs = acc.funcs ++ [fun]}
              Right externFun -> acc {externFuns = acc.externFuns ++ [externFun]}
          )
          initialProgram
          defs

  return programResult

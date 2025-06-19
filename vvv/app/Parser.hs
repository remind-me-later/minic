{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Parser (program) where

import Ast qualified
  ( Block (..),
    Exp (..),
    ExpInner (..),
    Fun (..),
    Id,
    Operator (..),
    Program (..),
    RawBlock,
    RawExp,
    RawFun,
    RawProgram,
    RawStmt,
    Stmt (..),
    Ty (..),
    VarDef (..),
  )
import Control.Applicative (Alternative (many, (<|>)), liftA2, optional)
import Data.Functor (($>))
import Text.ParserCombinators.Parsec qualified as PC

lex :: PC.Parser a -> PC.Parser a
lex p = p <* PC.spaces

keyword :: String -> PC.Parser String
keyword kw = Parser.lex (PC.string kw)

ifkw :: PC.Parser String
ifkw = keyword "if"

whilekw :: PC.Parser String
whilekw = keyword "while"

elsekw :: PC.Parser String
elsekw = keyword "else"

returnkw :: PC.Parser String
returnkw = keyword "return"

symbol :: String -> PC.Parser String
symbol sym = Parser.lex (PC.string sym)

parens :: PC.Parser a -> PC.Parser a
parens p = symbol "(" *> p <* symbol ")"

braces :: PC.Parser a -> PC.Parser a
braces p = symbol "{" *> p <* symbol "}"

commaSep :: PC.Parser a -> PC.Parser [a]
commaSep p = PC.sepBy p (symbol ",")

id :: PC.Parser Ast.Id
id =
  let isFirstChar c = c `elem` ['a' .. 'z'] ++ ['A' .. 'Z'] ++ "_"
      isOtherChar c = isFirstChar c || c `elem` ['0' .. '9']
   in Parser.lex (liftA2 (:) (PC.satisfy isFirstChar) (many (PC.satisfy isOtherChar)))

ty :: PC.Parser Ast.Ty
ty =
  let parseInt = (keyword "int" $> Ast.IntTy)
      parseBool = (keyword "bool" $> Ast.BoolTy)
      parseVoid = (keyword "void" $> Ast.VoidTy)
   in Parser.lex (parseInt <|> parseBool <|> parseVoid)

num :: PC.Parser Int
num =
  let isDigit c = c `elem` ['0' .. '9']
   in read <$> Parser.lex (PC.many1 (PC.satisfy isDigit) >>= \digits -> return digits)

exp :: PC.Parser Ast.RawExp
exp = eqexp
  where
    factor =
      PC.try callexp
        <|> PC.try idexp
        <|> PC.try parenexp
        <|> PC.try numexp
      where
        numexp = do
          num <- num
          return Ast.Exp {annot = (), exp = Ast.NumberExp {num}}
        idexp = do
          id <- Parser.id
          return Ast.Exp {annot = (), exp = Ast.IdExp {id}}
        parenexp = parens Parser.exp
        callexp = do
          id <- Parser.id
          args <- parens (commaSep Parser.exp)
          return Ast.Exp {annot = (), exp = Ast.Call {id, args}}

    mulexp = do
      left <- factor
      maybeOp <- optional (symbol "*" $> Ast.Mul)
      case maybeOp of
        Just op -> do
          right <- mulexp
          return Ast.Exp {annot = (), exp = Ast.BinExp {left, op, right}}
        Nothing -> return left

    addsubexp = do
      left <- mulexp
      maybeOp <-
        optional
          ( symbol "+" $> Ast.Add
              <|> symbol "-" $> Ast.Sub
          )
      case maybeOp of
        Just op -> do
          right <- addsubexp
          return Ast.Exp {annot = (), exp = Ast.BinExp {left, op, right}}
        Nothing -> return left

    eqexp = do
      left <- addsubexp
      maybeOp <-
        optional
          ( symbol "==" $> Ast.Equal
              <|> symbol "!=" $> Ast.NotEqual
              <|> symbol "<" $> Ast.LessThan
              <|> symbol ">" $> Ast.GreaterThan
              <|> symbol "<=" $> Ast.LessThanOrEqual
              <|> symbol ">=" $> Ast.GreaterThanOrEqual
          )
      case maybeOp of
        Just op -> do
          right <- eqexp
          return Ast.Exp {annot = (), exp = Ast.BinExp {left, op, right}}
        Nothing -> return left

vardef :: PC.Parser Ast.VarDef
vardef = do
  ty <- ty
  id <- Parser.id
  return Ast.VarDef {id, ty}

stmt :: PC.Parser Ast.RawStmt
stmt =
  PC.try letstmt
    <|> PC.try retstmt
    <|> PC.try ifstmt
    <|> PC.try whilestmt
    <|> PC.try assignstmt
    <|> PC.try expstmt
  where
    letstmt = do
      vardef <- vardef
      _ <- symbol "="
      exp <- Parser.exp
      _ <- symbol ";"
      return $ Ast.LetStmt {vardef, exp}

    assignstmt = do
      id <- Parser.id
      _ <- symbol "="
      exp <- Parser.exp
      _ <- symbol ";"
      return $ Ast.AssignStmt {id, exp}

    retstmt = do
      _ <- returnkw
      retexp <- optional Parser.exp
      _ <- symbol ";"
      return $ Ast.ReturnStmt {retexp}

    expstmt = do
      exp <- Parser.exp
      _ <- symbol ";"
      return Ast.ExpStmt {exp}

    ifstmt = do
      _ <- ifkw
      cond <- parens Parser.exp
      ifBody <- block
      elseBody <- optional (elsekw *> block)
      return $ Ast.IfStmt {cond, ifBody, elseBody}

    whilestmt = do
      _ <- whilekw
      cond <- Parser.exp
      body <- block
      return Ast.WhileStmt {cond, body}

block :: PC.Parser Ast.RawBlock
block = do
  stmts <- braces (many stmt)
  return $ Ast.Block {annot = (), stmts}

fun :: PC.Parser Ast.RawFun
fun = do
  retty <- ty
  id <- Parser.id
  args <- parens (commaSep vardef)
  body <- block
  return Ast.Fun {id, args, retty, body}

program :: PC.Parser Ast.RawProgram
program = do
  _ <- PC.spaces
  funcs <- many fun
  _ <- PC.eof
  return Ast.Program {annot = (), funcs}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Ast.Parse (program) where

import Ast.Types qualified
import Control.Applicative (Alternative (many, (<|>)), liftA2, optional)
import Data.Functor (($>))
import Text.ParserCombinators.Parsec qualified as PC

comment :: PC.Parser ()
comment = PC.try (PC.string "//" *> PC.many (PC.satisfy (/= '\n')) *> PC.optional (PC.char '\n')) $> ()

lex :: PC.Parser a -> PC.Parser a
lex p = p <* PC.spaces <* PC.optional comment

keyword :: String -> PC.Parser String
keyword kw = Ast.Parse.lex (PC.string kw)

ifkw :: PC.Parser String
ifkw = keyword "if"

whilekw :: PC.Parser String
whilekw = keyword "while"

elsekw :: PC.Parser String
elsekw = keyword "else"

returnkw :: PC.Parser String
returnkw = keyword "return"

symbol :: String -> PC.Parser String
symbol sym = Ast.Parse.lex (PC.string sym)

parens :: PC.Parser a -> PC.Parser a
parens p = symbol "(" *> p <* symbol ")"

braces :: PC.Parser a -> PC.Parser a
braces p = symbol "{" *> p <* symbol "}"

commaSep :: PC.Parser a -> PC.Parser [a]
commaSep p = PC.sepBy p (symbol ",")

id :: PC.Parser Ast.Types.Id
id =
  let isFirstChar c = c `elem` ['a' .. 'z'] ++ ['A' .. 'Z'] ++ "_"
      isOtherChar c = isFirstChar c || c `elem` ['0' .. '9']
   in Ast.Parse.lex (liftA2 (:) (PC.satisfy isFirstChar) (many (PC.satisfy isOtherChar)))

ty :: PC.Parser Ast.Types.Ty
ty =
  let parseInt = (keyword "int" $> Ast.Types.IntTy)
      parseBool = (keyword "bool" $> Ast.Types.BoolTy)
      parseVoid = (keyword "void" $> Ast.Types.VoidTy)
   in Ast.Parse.lex (parseInt <|> parseBool <|> parseVoid)

num :: PC.Parser Int
num =
  let isDigit c = c `elem` ['0' .. '9']
   in read <$> Ast.Parse.lex (PC.many1 (PC.satisfy isDigit))

exp :: PC.Parser Ast.Types.RawExp
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
          return Ast.Types.Exp {annot = (), exp = Ast.Types.NumberExp {num}}
        idexp = do
          id <- Ast.Parse.id
          return Ast.Types.Exp {annot = (), exp = Ast.Types.IdExp {id}}
        parenexp = parens Ast.Parse.exp
        callexp = do
          id <- Ast.Parse.id
          args <- parens (commaSep Ast.Parse.exp)
          return Ast.Types.Exp {annot = (), exp = Ast.Types.Call {id, args}}

    mulexp = do
      left <- factor
      maybeOp <- optional (symbol "*" $> Ast.Types.Mul)
      case maybeOp of
        Just op -> do
          right <- mulexp
          return Ast.Types.Exp {annot = (), exp = Ast.Types.BinExp {left, op, right}}
        Nothing -> return left

    addsubexp = do
      left <- mulexp
      maybeOp <-
        optional
          ( symbol "+" $> Ast.Types.Add
              <|> symbol "-" $> Ast.Types.Sub
          )
      case maybeOp of
        Just op -> do
          right <- addsubexp
          return Ast.Types.Exp {annot = (), exp = Ast.Types.BinExp {left, op, right}}
        Nothing -> return left

    eqexp = do
      left <- addsubexp
      maybeOp <-
        optional
          ( PC.try (symbol "==" $> Ast.Types.Equal)
              <|> PC.try (symbol "!=" $> Ast.Types.NotEqual)
              <|> PC.try (symbol "<=" $> Ast.Types.LessThanOrEqual)
              <|> PC.try (symbol ">=" $> Ast.Types.GreaterThanOrEqual)
              <|> PC.try (symbol "<" $> Ast.Types.LessThan)
              <|> symbol ">" $> Ast.Types.GreaterThan
          )
      case maybeOp of
        Just op -> do
          right <- eqexp
          return Ast.Types.Exp {annot = (), exp = Ast.Types.BinExp {left, op, right}}
        Nothing -> return left

vardef :: PC.Parser Ast.Types.VarDef
vardef = do
  ty <- ty
  id <- Ast.Parse.id
  return Ast.Types.VarDef {id, ty}

stmt :: PC.Parser Ast.Types.RawStmt
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
      exp <- Ast.Parse.exp
      _ <- symbol ";"
      return $ Ast.Types.LetStmt {vardef, exp}

    assignstmt = do
      id <- Ast.Parse.id
      _ <- symbol "="
      exp <- Ast.Parse.exp
      _ <- symbol ";"
      return $ Ast.Types.AssignStmt {id, exp}

    retstmt = do
      _ <- returnkw
      retexp <- optional Ast.Parse.exp
      _ <- symbol ";"
      return $ Ast.Types.ReturnStmt {retexp}

    expstmt = do
      exp <- Ast.Parse.exp
      _ <- symbol ";"
      return Ast.Types.ExpStmt {exp}

    ifstmt = do
      _ <- ifkw
      cond <- parens Ast.Parse.exp
      ifBody <- block
      elseBody <- optional (elsekw *> block)
      return $ Ast.Types.IfStmt {cond, ifBody, elseBody}

    whilestmt = do
      _ <- whilekw
      cond <- Ast.Parse.exp
      body <- block
      return Ast.Types.WhileStmt {cond, body}

block :: PC.Parser Ast.Types.RawBlock
block = do
  stmts <- braces (many stmt)
  return $ Ast.Types.Block {annot = (), stmts}

fun :: PC.Parser Ast.Types.RawFun
fun = do
  retty <- ty
  id <- Ast.Parse.id
  args <- parens (commaSep vardef)
  body <- block
  return Ast.Types.Fun {id, args, retty, body}

externfun :: PC.Parser Ast.Types.RawExternFun
externfun = do
  _ <- keyword "extern"
  retty <- ty
  id <- Ast.Parse.id
  args <- parens (commaSep vardef)
  _ <- symbol ";"
  return
    Ast.Types.ExternFun
      { id,
        args = (.ty) <$> args,
        retty
      }

program :: PC.Parser Ast.Types.RawProgram
program = do
  _ <- PC.spaces
  defs <- many (PC.try (Left <$> fun) <|> (Right <$> externfun))
  _ <- PC.eof

  let initialProgram = Ast.Types.Program {annot = (), funcs = [], externFuns = [], mainFun = Nothing}
  let programResult =
        foldl
          ( \acc f -> case f of
              Left fun ->
                case fun.id of
                  "main" -> acc {Ast.Types.mainFun = Just fun}
                  _ -> acc {Ast.Types.funcs = Ast.Types.funcs acc ++ [fun]}
              Right externFun -> acc {Ast.Types.externFuns = Ast.Types.externFuns acc ++ [externFun]}
          )
          initialProgram
          defs

  return programResult

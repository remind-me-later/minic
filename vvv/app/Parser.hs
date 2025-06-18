{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Parser (parseProgram) where

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

parseId :: PC.Parser Ast.Id
parseId =
  let isFirstChar c = c `elem` ['a' .. 'z'] ++ ['A' .. 'Z'] ++ "_"
      isOtherChar c = isFirstChar c || c `elem` ['0' .. '9']
   in Parser.lex (liftA2 (:) (PC.satisfy isFirstChar) (many (PC.satisfy isOtherChar)))

parseTy :: PC.Parser Ast.Ty
parseTy =
  let parseInt = (keyword "int" $> Ast.IntTy)
      parseBool = (keyword "bool" $> Ast.BoolTy)
      parseVoid = (keyword "void" $> Ast.VoidTy)
   in Parser.lex (parseInt <|> parseBool <|> parseVoid)

parseNum :: PC.Parser Int
parseNum =
  let isDigit c = c `elem` ['0' .. '9']
   in read <$> Parser.lex (PC.many1 (PC.satisfy isDigit) >>= \digits -> return digits)

parseExp :: PC.Parser Ast.RawExp
parseExp = parseEqExp
  where
    parseFactor =
      PC.try parseCall
        <|> PC.try parseIdifierExp
        <|> PC.try parseParenExp
        <|> PC.try parseNumberExp
      where
        parseNumberExp = do
          num <- parseNum
          return Ast.Exp {annot = (), exp = Ast.NumberExp {num}}
        parseIdifierExp = do
          id <- parseId
          return Ast.Exp {annot = (), exp = Ast.IdifierExp {id}}
        parseParenExp = parens parseExp
        parseCall = do
          id <- parseId
          args <- parens (commaSep parseExp)
          return Ast.Exp {annot = (), exp = Ast.Call {id, args}}

    parseMulExp = do
      left <- parseFactor
      maybeOp <- optional (symbol "*" $> Ast.Mul)
      case maybeOp of
        Just op -> do
          right <- parseMulExp
          return Ast.Exp {annot = (), exp = Ast.BinExp {left, op, right}}
        Nothing -> return left

    parseAddSubExp = do
      left <- parseMulExp
      maybeOp <-
        optional
          ( symbol "+" $> Ast.Add
              <|> symbol "-" $> Ast.Sub
          )
      case maybeOp of
        Just op -> do
          right <- parseAddSubExp
          return Ast.Exp {annot = (), exp = Ast.BinExp {left, op, right}}
        Nothing -> return left

    parseEqExp = do
      left <- parseAddSubExp
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
          right <- parseEqExp
          return Ast.Exp {annot = (), exp = Ast.BinExp {left, op, right}}
        Nothing -> return left

parseVarDef :: PC.Parser Ast.VarDef
parseVarDef = do
  ty <- parseTy
  id <- parseId
  return Ast.VarDef {id, ty}

parseStmt :: PC.Parser Ast.RawStmt
parseStmt =
  PC.try parseLetStmt
    <|> PC.try parseReturnStmt
    <|> PC.try parseIfStmt
    <|> PC.try parseWhileStmt
    <|> PC.try parseAssignStmt
    <|> PC.try parseExpStmt
  where
    parseLetStmt = do
      vardef <- parseVarDef
      _ <- symbol "="
      exp <- parseExp
      _ <- symbol ";"
      return $ Ast.LetStmt {vardef, exp}

    parseAssignStmt = do
      id <- parseId
      _ <- symbol "="
      exp <- parseExp
      _ <- symbol ";"
      return $ Ast.AssignStmt {id, exp}

    parseReturnStmt = do
      _ <- returnkw
      retexp <- optional parseExp
      _ <- symbol ";"
      return $ Ast.ReturnStmt {retexp}

    parseExpStmt = do
      exp <- parseExp
      _ <- symbol ";"
      return Ast.ExpStmt {exp}

    parseIfStmt = do
      _ <- ifkw
      cond <- parens parseExp
      ifBody <- parseBlock
      elseBody <- optional (elsekw *> parseBlock)
      return $ Ast.IfStmt {cond, ifBody, elseBody}

    parseWhileStmt = do
      _ <- whilekw
      cond <- parseExp
      body <- parseBlock
      return Ast.WhileStmt {cond, body}

parseBlock :: PC.Parser Ast.RawBlock
parseBlock = do
  stmts <- braces (many parseStmt)
  return $ Ast.Block {annot = (), stmts}

parseFun :: PC.Parser Ast.RawFun
parseFun = do
  retty <- parseTy
  id <- parseId
  args <- parens (commaSep parseVarDef)
  body <- parseBlock
  return Ast.Fun {id, args, retty, body}

parseProgram :: PC.Parser Ast.RawProgram
parseProgram = do
  _ <- PC.spaces
  funcs <- many parseFun
  _ <- PC.eof
  return Ast.Program {funcs}

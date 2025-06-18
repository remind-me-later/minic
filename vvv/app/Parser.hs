{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Parser (parseProgram) where

import Ast
  ( Block (..),
    Exp (..),
    ExpInner (..),
    Fun (..),
    Ident,
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
import Control.Applicative (Alternative (many, (<|>)), optional)
import ParserCombinators qualified as PC (Parser (..), satisfy)
import Token qualified as TOK (Keyword (..), Punctuation (..), Token (..))

type TParser o = PC.Parser [TOK.Token] o

parseIdent :: TParser Ident
parseIdent =
  PC.Parser
    ( \case
        TOK.Identifier s : rest -> Just (s, rest)
        _ -> Nothing
    )

parseTy :: TParser Ty
parseTy =
  PC.Parser
    ( \case
        TOK.Identifier s : rest ->
          ( case s of
              "int" -> Just (Int, rest)
              "bool" -> Just (Bool, rest)
              "void" -> Just (Void, rest)
              _ -> Nothing
          )
        _ -> Nothing
    )

parseNum :: TParser Int
parseNum =
  PC.Parser
    ( \case
        TOK.Number n : rest -> Just (n, rest)
        _ -> Nothing
    )

parseOp :: Operator -> TParser Operator
parseOp op =
  PC.Parser
    ( \case
        TOK.Operator op' : rest
          | op == op' ->
              Just (op, rest)
        _ -> Nothing
    )

parens :: TParser o -> TParser o
parens p =
  PC.satisfy (== TOK.Punctuation TOK.LeftParen)
    *> p
    <* PC.satisfy (== TOK.Punctuation TOK.RightParen)

braces :: TParser o -> TParser o
braces p =
  PC.satisfy (== TOK.Punctuation TOK.LeftBrace)
    *> p
    <* PC.satisfy (== TOK.Punctuation TOK.RightBrace)

parseExp :: TParser RawExp
parseExp = parseEqExp
  where
    parseFactor = parseCall <|> parseIdentifierExp <|> parseParenExp <|> parseNumberExp
      where
        parseNumberExp = do
          num <- parseNum
          return Exp {annot = (), exp = NumberExp {num}}
        parseIdentifierExp = do
          id <- parseIdent
          return Exp {annot = (), exp = IdentifierExp {id}}
        parseParenExp = parens parseExp
        parseCall = do
          id <- parseIdent
          _ <- PC.satisfy (== TOK.Punctuation TOK.LeftParen)
          args <- parseCommaSeparated0 parseExp
          _ <- PC.satisfy (== TOK.Punctuation TOK.RightParen)
          return Exp {annot = (), exp = Call {id, args}}

    parseMulExp = do
      left <- parseFactor
      maybeOp <- optional (parseOp Multiply)
      case maybeOp of
        Just op -> do
          right <- parseMulExp
          return Exp {annot = (), exp = BinExp {left, op, right}}
        Nothing -> return left

    parseAddSubExp = do
      left <- parseMulExp
      maybeOp <-
        optional
          ( parseOp Add
              <|> parseOp Subtract
          )
      case maybeOp of
        Just op -> do
          right <- parseAddSubExp
          return Exp {annot = (), exp = BinExp {left, op, right}}
        Nothing -> return left

    parseEqExp = do
      left <- parseAddSubExp
      maybeOp <-
        optional
          ( parseOp Equal
              <|> parseOp NotEqual
              <|> parseOp LessThan
              <|> parseOp GreaterThan
              <|> parseOp LessThanOrEqual
              <|> parseOp GreaterThanOrEqual
          )
      case maybeOp of
        Just op -> do
          right <- parseEqExp
          return Exp {annot = (), exp = BinExp {left, op, right}}
        Nothing -> return left

parseVarDef :: TParser VarDef
parseVarDef = do
  ty <- parseTy
  id <- parseIdent
  return VarDef {id, ty}

parseStmt :: TParser RawStmt
parseStmt =
  parseLetStmt
    <|> parseReturnStmt
    <|> parseIfStmt
    <|> parseWhileStmt
    <|> parseAssignStmt
    <|> parseExpStmt
  where
    parseLetStmt = do
      vardef <- parseVarDef
      _ <- PC.satisfy (== TOK.Operator Assign)
      exp <- parseExp
      _ <- PC.satisfy (== TOK.Punctuation TOK.SemiColon)
      return $ LetStmt {vardef, exp}

    parseAssignStmt = do
      id <- parseIdent
      _ <- PC.satisfy (== TOK.Operator Assign)
      exp <- parseExp
      _ <- PC.satisfy (== TOK.Punctuation TOK.SemiColon)
      return $ AssignStmt {id, exp}

    parseReturnStmt = do
      _ <- PC.satisfy (== TOK.Keyword TOK.Return)
      exp <- parseExp
      _ <- PC.satisfy (== TOK.Punctuation TOK.SemiColon)
      return $ ReturnStmt {exp}

    parseExpStmt = do
      exp <- parseExp
      _ <- PC.satisfy (== TOK.Punctuation TOK.SemiColon)
      return ExpStmt {exp}

    parseIfStmt = do
      _ <- PC.satisfy (== TOK.Keyword TOK.If)
      cond <- parens parseExp
      ifBody <- parseBlock
      elseBody <- optional (PC.satisfy (== TOK.Keyword TOK.Else) *> parseBlock)
      return $ IfStmt {cond, ifBody, elseBody}

    parseWhileStmt = do
      _ <- PC.satisfy (== TOK.Keyword TOK.While)
      cond <- parseExp
      body <- parseBlock
      return WhileStmt {cond, body}

parseBlock :: TParser RawBlock
parseBlock = do
  stmts <- braces (many parseStmt)
  return $ Block {annot = (), stmts}

parseCommaSeparated :: TParser a -> TParser [a]
parseCommaSeparated p = p `parseSepBy` PC.satisfy (== TOK.Punctuation TOK.Comma)
  where
    parseSepBy :: TParser a -> TParser b -> TParser [a]
    parseSepBy p' sep = (:) <$> p' <*> many (sep *> p')

parseCommaSeparated0 :: TParser a -> TParser [a]
parseCommaSeparated0 p = parseCommaSeparated p <|> pure []

parseFun :: TParser RawFun
parseFun = do
  retty <- parseTy
  id <- parseIdent
  args <- parens (parseCommaSeparated0 parseVarDef)
  body <- parseBlock
  return Ast.Fun {id, args, retty, body}

parseProgram :: TParser RawProgram
parseProgram = Program <$> many parseFun

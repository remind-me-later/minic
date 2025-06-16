{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Parser (parseProgram) where

import Ast (Block (..), Expr (..), Fun (..), Ident, Operator (..), Program (..), RawBlock, RawExpr, RawFun, RawProgram, RawStmt, Stmt (..), Ty (..), VarDef (..))
import Control.Applicative (Alternative (many, (<|>)), optional)
import Data.Maybe (fromMaybe)
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
              "i32" -> Just (I32, rest)
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

parseExpr :: TParser RawExpr
parseExpr = parseEqExpr
  where
    parseFactor = parseCall <|> parseIdentifierExpr <|> parseParenExpr <|> parseNumberExpr
      where
        parseNumberExpr = NumberExpr () <$> parseNum
        parseIdentifierExpr = IdentifierExpr () <$> parseIdent
        parseParenExpr =
          PC.satisfy
            (== TOK.Punctuation TOK.LeftParen)
            *> parseExpr
            <* PC.satisfy (== TOK.Punctuation TOK.RightParen)
        parseCall =
          Call ()
            <$> parseIdent
            <*> ( PC.satisfy
                    (== TOK.Punctuation TOK.LeftParen)
                    *> parseCommaSeparated0 parseExpr
                    <* PC.satisfy (== TOK.Punctuation TOK.RightParen)
                )

    parseMulExpr = do
      left <- parseFactor
      maybeOp <- optional (parseOp Multiply)
      case maybeOp of
        Just op -> BinExpr () left op <$> parseMulExpr
        Nothing -> return left

    parseAddSubExpr = do
      left <- parseMulExpr
      maybeOp <-
        optional
          ( parseOp Add
              <|> parseOp Subtract
          )
      case maybeOp of
        Just op -> BinExpr () left op <$> parseAddSubExpr
        Nothing -> return left

    parseEqExpr = do
      left <- parseAddSubExpr
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
        Just op -> BinExpr () left op <$> parseEqExpr
        Nothing -> return left

parseVarDef :: TParser VarDef
parseVarDef =
  VarDef
    <$> parseIdent
    <*> (PC.satisfy (== TOK.Punctuation TOK.Colon) *> parseTy)

parseStmt :: TParser RawStmt
parseStmt =
  ( parseLetStmt
      <|> parseReturnStmt
      <|> parseIfStmt
      <|> parseWhileStmt
      <|> parseAssignStmt
      <|> parseExprStmt
  )
    <* PC.satisfy (== TOK.Punctuation TOK.SemiColon)
  where
    parseLetStmt =
      LetStmt
        <$> (PC.satisfy (== TOK.Keyword TOK.Let) *> parseVarDef <* PC.satisfy (== TOK.Operator Assign))
        <*> parseExpr
    parseAssignStmt =
      AssignStmt
        <$> (parseIdent <* PC.satisfy (== TOK.Operator Assign))
        <*> parseExpr
    parseReturnStmt =
      ReturnStmt
        <$> (PC.satisfy (== TOK.Keyword TOK.Return) *> parseExpr)
    parseExprStmt = ExprStmt <$> parseExpr
    parseIfStmt = do
      _ <- PC.satisfy (== TOK.Keyword TOK.If)
      cond <- parseExpr
      ifBody <- parseBlock
      elseBody <- optional (PC.satisfy (== TOK.Keyword TOK.Else) *> parseBlock)
      return $ IfStmt {cond, ifBody, elseBody}
    parseWhileStmt = do
      _ <- PC.satisfy (== TOK.Keyword TOK.While)
      cond <- parseExpr
      body <- parseBlock
      return WhileStmt {cond, body}

parseBlock :: TParser RawBlock
parseBlock =
  Block ()
    <$> ( PC.satisfy
            (== TOK.Punctuation TOK.LeftBrace)
            *> many parseStmt
            <* PC.satisfy (== TOK.Punctuation TOK.RightBrace)
        )

parseCommaSeparated :: TParser a -> TParser [a]
parseCommaSeparated p = p `parseSepBy` PC.satisfy (== TOK.Punctuation TOK.Comma)
  where
    parseSepBy :: TParser a -> TParser b -> TParser [a]
    parseSepBy p' sep = (:) <$> p' <*> many (sep *> p')

parseCommaSeparated0 :: TParser a -> TParser [a]
parseCommaSeparated0 p = parseCommaSeparated p <|> pure []

parseFun :: TParser RawFun
parseFun = do
  _ <- PC.satisfy (== TOK.Keyword TOK.Fun)
  id <- parseIdent
  args <-
    PC.satisfy
      (== TOK.Punctuation TOK.LeftParen)
      *> parseCommaSeparated0 parseVarDef
      <* PC.satisfy (== TOK.Punctuation TOK.RightParen)
  retty <- optional parseTy
  body <- parseBlock
  return Ast.Fun {id, args, retty = fromMaybe Void retty, body}

parseProgram :: TParser RawProgram
parseProgram = Program <$> many parseFun

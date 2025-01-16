{-# LANGUAGE LambdaCase #-}

module Parser (Expr (..), Stmt (..), Block (..), Fun (..), TParser, Program (..), program) where

import Control.Applicative (Alternative (many, (<|>)), optional)
import ParserCombinators (Parser (..), satisfy)
import Token (Keyword (..), Operator (..), Punctuation (..), Token (..))

data Expr
  = BinExpr Expr Operator Expr
  | NumberExpr Int
  | IdentifierExpr String
  | Call String [Expr]
  deriving (Show, Eq)

data VarDef = VarDef String String deriving (Show, Eq)

data Stmt
  = ExprStmt Expr
  | LetStmt VarDef Expr
  | AssignStmt String Expr
  | ReturnStmt Expr
  | IfStmt Expr Block (Maybe Block)
  | ForStmt VarDef Expr Expr Block
  deriving (Show, Eq)

data Block = Block [Stmt] deriving (Show, Eq)

data Fun = Fun String [VarDef] Block deriving (Show, Eq)

data Program = Program [Fun] deriving (Show, Eq)

type TParser o = Parser [Token] o

identifier :: TParser String
identifier = Parser (\case Identifier s : rest -> Just (s, rest); _ -> Nothing)

number :: TParser Int
number = Parser (\case Number n : rest -> Just (n, rest); _ -> Nothing)

operator :: Operator -> TParser Operator
operator op = Parser (\case Operator op' : rest | op == op' -> Just (op, rest); _ -> Nothing)

expr :: TParser Expr
expr = eqExpr
  where
    factor = call <|> identifierExpr <|> parenExpr <|> numberExpr
      where
        numberExpr = NumberExpr <$> number
        identifierExpr = IdentifierExpr <$> identifier
        parenExpr = satisfy (== Punctuation LeftParen) *> expr <* satisfy (== Punctuation RightParen)
        call = Call <$> identifier <*> (satisfy (== Punctuation LeftParen) *> commaSeparated0 expr <* satisfy (== Punctuation RightParen))

    mulExpr = do
      left <- factor
      maybeOp <- optional (operator Multiply)
      case maybeOp of
        Just op -> BinExpr left op <$> mulExpr
        Nothing -> return left

    addSubExpr = do
      left <- mulExpr
      maybeOp <- optional (operator Add <|> operator Subtract)
      case maybeOp of
        Just op -> BinExpr left op <$> addSubExpr
        Nothing -> return left

    eqExpr = do
      left <- addSubExpr
      maybeOp <- optional (operator Equal)
      case maybeOp of
        Just op -> BinExpr left op <$> eqExpr
        Nothing -> return left

varDef :: TParser VarDef
varDef = VarDef <$> identifier <*> (satisfy (== Punctuation Colon) *> identifier)

stmt :: TParser Stmt
stmt = (letStmt <|> returnStmt <|> ifStmt <|> forStmt <|> assignStmt <|> exprStmt) <* satisfy (== Punctuation SemiColon)
  where
    letStmt = LetStmt <$> (satisfy (== Keyword Let) *> varDef <* satisfy (== Operator Assign)) <*> expr
    assignStmt = AssignStmt <$> (identifier <* satisfy (== Operator Assign)) <*> expr
    returnStmt = ReturnStmt <$> (satisfy (== Keyword Return) *> expr)
    exprStmt = ExprStmt <$> expr
    ifStmt = do
      _ <- satisfy (== Keyword If)
      cond <- expr
      ifBlock <- block
      elseBlock <- optional (satisfy (== Keyword Else) *> block)
      return $ IfStmt cond ifBlock elseBlock
    forStmt = do
      _ <- satisfy (== Keyword For)
      var <- varDef
      _ <- satisfy (== Keyword In)
      begin <- expr
      _ <- satisfy (== Operator Range)
      end <- expr
      ForStmt var begin end <$> block

block :: TParser Block
block = Block <$> (satisfy (== Punctuation LeftBrace) *> many stmt <* satisfy (== Punctuation RightBrace))

commaSeparated :: TParser a -> TParser [a]
commaSeparated p = p `sepBy` satisfy (== Punctuation Comma)
  where
    sepBy :: TParser a -> TParser b -> TParser [a]
    sepBy p' sep = (:) <$> p' <*> many (sep *> p')

commaSeparated0 :: TParser a -> TParser [a]
commaSeparated0 p = commaSeparated p <|> pure []

fun :: TParser Fun
fun =
  Parser.Fun
    <$> (satisfy (== Keyword Token.Fun) *> identifier)
    <*> (satisfy (== Punctuation LeftParen) *> commaSeparated0 varDef <* satisfy (== Punctuation RightParen))
    <*> block

program :: TParser Program
program = Program <$> many fun

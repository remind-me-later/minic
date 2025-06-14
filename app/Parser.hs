{-# LANGUAGE LambdaCase #-}

module Parser (Expr (..), Stmt (..), Block (..), Fun (..), TParser, Program (..), program) where

import Ast (Block (..), Expr (..), Fun (..), Ident, Operator (..), Program (..), Stmt (..), Ty (..), VarDef (..))
import Control.Applicative (Alternative (many, (<|>)), optional)
import Data.Maybe (fromMaybe)
import ParserCombinators qualified as PC (Parser (..), satisfy)
import Token qualified as TOK (Keyword (..), Punctuation (..), Token (..))

type TParser o = PC.Parser [TOK.Token] o

identifier :: TParser Ident
identifier = PC.Parser (\case TOK.Identifier s : rest -> Just (s, rest); _ -> Nothing)

ty :: TParser Ty
ty =
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

number :: TParser Int
number = PC.Parser (\case TOK.Number n : rest -> Just (n, rest); _ -> Nothing)

operator :: Operator -> TParser Operator
operator op = PC.Parser (\case TOK.Operator op' : rest | op == op' -> Just (op, rest); _ -> Nothing)

expr :: TParser Expr
expr = eqExpr
  where
    factor = call <|> identifierExpr <|> parenExpr <|> numberExpr
      where
        numberExpr = NumberExpr <$> number
        identifierExpr = IdentifierExpr <$> identifier
        parenExpr = PC.satisfy (== TOK.Punctuation TOK.LeftParen) *> expr <* PC.satisfy (== TOK.Punctuation TOK.RightParen)
        call = Call <$> identifier <*> (PC.satisfy (== TOK.Punctuation TOK.LeftParen) *> commaSeparated0 expr <* PC.satisfy (== TOK.Punctuation TOK.RightParen))

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
      maybeOp <- optional (operator Equal <|> operator NotEqual <|> operator LessThan <|> operator GreaterThan <|> operator LessThanOrEqual <|> operator GreaterThanOrEqual)
      case maybeOp of
        Just op -> BinExpr left op <$> eqExpr
        Nothing -> return left

varDef :: TParser VarDef
varDef = VarDef <$> identifier <*> (PC.satisfy (== TOK.Punctuation TOK.Colon) *> ty)

stmt :: TParser Stmt
stmt = (letStmt <|> returnStmt <|> ifStmt <|> whileStmt <|> assignStmt <|> exprStmt) <* PC.satisfy (== TOK.Punctuation TOK.SemiColon)
  where
    letStmt = LetStmt <$> (PC.satisfy (== TOK.Keyword TOK.Let) *> varDef <* PC.satisfy (== TOK.Operator Assign)) <*> expr
    assignStmt = AssignStmt <$> (identifier <* PC.satisfy (== TOK.Operator Assign)) <*> expr
    returnStmt = ReturnStmt <$> (PC.satisfy (== TOK.Keyword TOK.Return) *> expr)
    exprStmt = ExprStmt <$> expr
    ifStmt = do
      _ <- PC.satisfy (== TOK.Keyword TOK.If)
      cond <- expr
      ifBlock <- block
      elseBlock <- optional (PC.satisfy (== TOK.Keyword TOK.Else) *> block)
      return $ IfStmt cond ifBlock elseBlock
    whileStmt = do
      _ <- PC.satisfy (== TOK.Keyword TOK.While)
      cond <- expr
      WhileStmt cond <$> block

block :: TParser Block
block = Block <$> (PC.satisfy (== TOK.Punctuation TOK.LeftBrace) *> many stmt <* PC.satisfy (== TOK.Punctuation TOK.RightBrace))

commaSeparated :: TParser a -> TParser [a]
commaSeparated p = p `sepBy` PC.satisfy (== TOK.Punctuation TOK.Comma)
  where
    sepBy :: TParser a -> TParser b -> TParser [a]
    sepBy p' sep = (:) <$> p' <*> many (sep *> p')

commaSeparated0 :: TParser a -> TParser [a]
commaSeparated0 p = commaSeparated p <|> pure []

fun :: TParser Fun
fun = do
  _ <- PC.satisfy (== TOK.Keyword TOK.Fun)
  funName <- identifier
  funArgs <- PC.satisfy (== TOK.Punctuation TOK.LeftParen) *> commaSeparated0 varDef <* PC.satisfy (== TOK.Punctuation TOK.RightParen)
  funRetTy <- optional ty
  funBody <- block
  return Ast.Fun {funName, funArgs, funRetTy = fromMaybe Void funRetTy, funBody}

program :: TParser Program
program = Program <$> many fun

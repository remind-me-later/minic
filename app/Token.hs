module Token
  ( Token (..),
    Operator (..),
    Keyword (..),
    Punctuation (..),
    tokens,
  )
where

import Control.Applicative (Alternative (many, some, (<|>)))
import Data.Functor (($>))
import ParserCombinators (Parser, char, satisfy, string)

-- Example vvc program:
-- fun main() {
--  let x = 1;
--  let y = 2;
--  let z = x + y;
--  return z;
-- }

data Operator
  = Add
  | Subtract
  | Multiply
  | Assign
  | Equal
  | Range
  deriving (Show, Eq)

data Keyword
  = Let
  | Cons
  | Fun
  | If
  | Else
  | While
  | For
  | Return
  | In
  deriving (Show, Eq)

data Punctuation
  = Comma
  | Colon
  | SemiColon
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  deriving (Show, Eq)

data Token
  = Identifier String
  | Number Int
  | Operator Operator
  | Keyword Keyword
  | Punctuation Punctuation
  deriving (Show, Eq)

type SParser o = Parser String o

space :: SParser Char
space = satisfy (`elem` " \t\n")

spaces :: SParser String
spaces = many space

number :: SParser Int
number = read <$> some (satisfy (`elem` ['0' .. '9']))

identifier :: SParser String
identifier = do
  c <- satisfy (`elem` ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['_'])
  cs <- many (satisfy (`elem` ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['_']))
  return (c : cs)

operator :: SParser Operator
operator = do
  op <- string "+" <|> string "-" <|> string ".." <|> string "*" <|> string "==" <|> string "="
  return $ case op of
    "+" -> Add
    "-" -> Subtract
    "*" -> Multiply
    ".." -> Range
    "==" -> Equal
    "=" -> Assign
    _ -> error "unreachable"

keyword :: SParser Keyword
keyword = do
  kw <- (string "let" <|> string "const" <|> string "in" <|> string "fun" <|> string "if" <|> string "else" <|> string "while" <|> string "for" <|> string "return") <* space
  return $ case kw of
    "let" -> Let
    "const" -> Cons
    "fun" -> Fun
    "if" -> If
    "else" -> Else
    "while" -> While
    "for" -> For
    "return" -> Return
    "in" -> In
    _ -> error "unreachable"

punctuation :: SParser Punctuation
punctuation =
  (char ',' $> Comma)
    <|> (char ':' $> Colon)
    <|> (char ';' $> SemiColon)
    <|> (char '(' $> LeftParen)
    <|> (char ')' $> RightParen)
    <|> (char '{' $> LeftBrace)
    <|> (char '}' $> RightBrace)

token :: SParser Token
token =
  spaces
    *> ( Number <$> number
           <|> Keyword <$> keyword
           <|> Identifier <$> identifier
           <|> Operator <$> operator
           <|> Punctuation <$> punctuation
       )

tokens :: SParser [Token]
tokens = many token
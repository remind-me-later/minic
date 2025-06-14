module Token
  ( Token (..),
    Keyword (..),
    Punctuation (..),
    tokens,
  )
where

import Ast (Operator (..))
import Control.Applicative (Alternative (many, some, (<|>)))
import Data.Functor (($>))
import ParserCombinators (Parser, char, satisfy, string)

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
operator =
  (string "+" $> Add)
    <|> (string "-" $> Subtract)
    <|> (string "*" $> Multiply)
    <|> (string "==" $> Equal)
    <|> (string "=" $> Assign)
    <|> (string "<=" $> LessThanOrEqual)
    <|> (string ">=" $> GreaterThanOrEqual)
    <|> (string "<" $> LessThan)
    <|> (string ">" $> GreaterThan)
    <|> (string "!=" $> NotEqual)
    <|> (string "&&" $> And)
    <|> (string "||" $> Or)
    <|> (string "!" $> Not)
    <|> (string "^" $> Xor)
    <|> (string "%" $> Modulo)

keyword :: SParser Keyword
keyword =
  (string "let" $> Let)
    <|> (string "const" $> Cons)
    <|> (string "func" $> Fun)
    <|> (string "if" $> If)
    <|> (string "else" $> Else)
    <|> (string "while" $> While)
    <|> (string "for" $> For)
    <|> (string "return" $> Return)
    <|> (string "in" $> In)
      <* space

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
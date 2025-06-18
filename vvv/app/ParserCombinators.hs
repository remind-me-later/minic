{-# LANGUAGE LambdaCase #-}

module ParserCombinators
  ( Parser (..),
    satisfy,
    char,
    string,
  )
where

import Control.Applicative

newtype Parser i o = Parser {parse :: i -> Maybe (o, i)}

instance Functor (Parser i) where
  fmap f (Parser p) = Parser $ \input -> do
    (x, rest) <- p input
    Just (f x, rest)

instance Applicative (Parser i) where
  pure :: a -> Parser i a
  pure x = Parser $ \input -> Just (x, input)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (f, rest1) <- p1 input
    (x, rest2) <- p2 rest1
    Just (f x, rest2)

instance Alternative (Parser i) where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

instance Monad (Parser i) where
  return = pure
  (Parser p) >>= f = Parser $ \input -> do
    (x, rest) <- p input
    parse (f x) rest

satisfy :: (a -> Bool) -> Parser [a] a
satisfy f = Parser $ \case
  c : rest | f c -> Just (c, rest)
  _ -> Nothing

char :: Char -> Parser String Char
char c = satisfy (== c)

string :: String -> Parser String String
string = traverse char
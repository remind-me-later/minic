{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}

module Ast.Recursion where

import Ast
import Data.Functor.Foldable
import TypeSystem

data ExpF ea r
  = BinExpF r BinOp r
  | UnaryExpF UnaryOp r
  | NumberExpF Int
  | CharExpF Char
  | IdExpF Id
  | CallF Id [r]
  deriving (Functor, Foldable, Traversable)

newtype ExpRec ea = ExpRec {unExpRec :: Exp ea}

type instance Base (ExpRec ea) = ExpF ea

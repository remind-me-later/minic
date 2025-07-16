{-# LANGUAGE TemplateHaskell #-}

module Ast.Lenses where

import Ast.Types
import Control.Lens

-- Generate lenses for all record fields
makeLenses ''Exp
makeLenses ''VarDef
makeLenses ''Block
makeLenses ''Fun
makeLenses ''ExternFun
makeLenses ''Program

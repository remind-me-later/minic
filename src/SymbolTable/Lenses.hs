{-# LANGUAGE TemplateHaskell #-}

module SymbolTable.Lenses where

import Control.Lens
import SymbolTable.Types

makeLenses ''VarSymbolStorage
makeLenses ''FunSymbolStorage
makeLenses ''ArgSymbolStorage
makeLenses ''Symbol
makeLenses ''Environment
makeLenses ''SymbolTable

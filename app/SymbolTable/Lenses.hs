{-# LANGUAGE TemplateHaskell #-}

module SymbolTable.Lenses where

import Control.Lens
import SymbolTable.Types

makeLenses ''Symbol
makeLenses ''Environment
makeLenses ''SymbolTable

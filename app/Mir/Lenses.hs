{-# LANGUAGE TemplateHaskell #-}

module Mir.Lenses where

import Control.Lens
import Mir.Types

makeLenses ''Temp
makeLenses ''Register
makeLenses ''Operand
makeLenses ''Terminator
makeLenses ''Inst
makeLenses ''BasicBlock
makeLenses ''CFG
makeLenses ''Fun
makeLenses ''ExternFun
makeLenses ''Program

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Mir.Types where

import Data.Set
import Env qualified
import TypeSystem

type Temp = Int

type BlockId = String

data Var
  = Local {id :: Id}
  | LocalWithOffset {id :: Id, offset :: Operand, mult :: Int}
  | Arg {id :: Id}
  deriving (Eq)

instance Show Var where
  show (Local id) = "local " ++ id
  show (Arg id) = "arg " ++ id
  show (LocalWithOffset id offset 0) = "local " ++ id ++ "[" ++ show offset ++ "]"
  show (LocalWithOffset id offset mult) =
    "local " ++ id ++ "[" ++ show offset ++ " * " ++ show mult ++ "]"

data Operand
  = ConstInt Int
  | ConstChar Char
  | Temp Temp
  deriving (Eq)

instance Show Operand where
  show (ConstInt n) = "const " ++ show n
  show (ConstChar c) = "const " ++ show c
  show (Temp t) = "t" ++ show t

data Terminator
  = Return {retVal :: Maybe Temp}
  | Jump {target :: BlockId}
  | CondJump {cond :: Temp, trueBlockId :: BlockId, falseBlockId :: BlockId}
  deriving (Eq)

instance Show Terminator where
  show (Return Nothing) = "return"
  show (Return (Just t)) = "return t" ++ show t
  show (Jump target) = "goto " ++ target
  show (CondJump cond trueBlockId falseBlockId) =
    "if t" ++ show cond ++ " then goto " ++ trueBlockId ++ " else goto " ++ falseBlockId

data Inst
  = Mov {dst :: Temp, srcOp :: Operand}
  | UnaryOp {dst :: Temp, unop :: UnaryOp, unsrc :: Operand}
  | BinOp {dst :: Temp, binop :: BinOp, left :: Operand, right :: Operand}
  | Load {dst :: Temp, srcVar :: Var}
  | Store {dstVar :: Var, src :: Temp}
  | Call {ret :: Maybe Temp, funId :: Id, argCount :: Int}
  | Param {param :: Temp}
  deriving (Eq)

instance Show Inst where
  show Mov {dst, srcOp} = "t" ++ show dst ++ " = " ++ show srcOp
  show UnaryOp {dst, unop, unsrc} = "t" ++ show dst ++ " = " ++ show unop ++ show unsrc
  show BinOp {dst, binop, left, right} =
    "t" ++ show dst ++ " = " ++ show left ++ " " ++ show binop ++ " " ++ show right
  show Load {dst, srcVar} = "t" ++ show dst ++ " = " ++ show srcVar
  show Store {dstVar, src} = show dstVar ++ " = t" ++ show src
  show Call {ret = Just t, funId, argCount} = "t" ++ show t ++ " = call " ++ funId ++ ", " ++ show argCount
  show Call {ret = Nothing, funId, argCount} = "call " ++ funId ++ ", " ++ show argCount
  show Param {param} = "param " ++ "t" ++ show param

-- A Basic Block is a sequence of instructions that starts with a blockId
-- and ends with a control flow instruction (Jump, CondJump, Return).
-- For simplicity here, we'll just list instructions and assume the last one is control flow.
-- A more rigorous CFG would explicitly link blocks.
data BasicBlock = BasicBlock
  { blockId :: BlockId,
    insts :: [Inst],
    terminator :: Terminator
  }
  deriving (Eq)

instance Show BasicBlock where
  show (BasicBlock blockId insts terminator) =
    blockId
      ++ ":\n"
      ++ unlines (("  " ++) . show <$> insts)
      ++ "  "
      ++ show terminator

data CFG = CFG
  { entryBlockId :: BlockId,
    exitBlocks :: Set BlockId,
    blocks :: [BasicBlock]
  }
  deriving (Eq)

instance Show CFG where
  show (CFG entryBlockId exitBlocks blocks) =
    "CFG:\n"
      ++ "Entry Block: "
      ++ entryBlockId
      ++ "\n"
      ++ "Exit Blocks: "
      ++ unwords (show <$> toList exitBlocks)
      ++ "\n"
      ++ "Blocks:\n"
      ++ unlines (show <$> blocks)

data Fun = Fun
  { id :: Id,
    args :: [Env.Symbol],
    locals :: [Env.Symbol],
    cfg :: CFG
  }
  deriving (Eq)

instance Show Fun where
  show (Fun id args locals cfg) =
    "Function: "
      ++ id
      ++ "\nArguments: "
      ++ unwords (show <$> args)
      ++ "\nLocals: "
      ++ unwords (show <$> locals)
      ++ "\n"
      ++ show cfg

newtype ExternFun = ExternFun
  { externId :: Id
  }
  deriving (Eq)

instance Show ExternFun where
  show (ExternFun id) = "Extern Function: " ++ id

data Program = Program
  { funs :: [Fun],
    externFuns :: [ExternFun],
    mainFun :: Maybe Fun
  }
  deriving (Eq)

instance Show Program where
  show (Program functions externFuns mainFun) =
    "Program:\n"
      ++ "Extern Functions:\n"
      ++ unlines (show <$> externFuns)
      ++ "Functions:\n"
      ++ unlines (show <$> functions)
      ++ case mainFun of
        Just main -> "Main Function:\n" ++ show main
        Nothing -> "No Main Function"

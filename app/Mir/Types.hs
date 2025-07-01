{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Mir.Types where

import Data.Set
import Env qualified
import TypeSystem

type Temp = Int

type BlockId = String

data Register = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
  deriving (Eq, Ord, Show, Enum, Bounded)

availableRegisters :: [Register]
availableRegisters = [R1 .. R8]

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
  | RegOperand Register
  | StackOperand Int
  deriving (Eq)

instance Show Operand where
  show (ConstInt n) = "const " ++ show n
  show (ConstChar c) = "const " ++ show c
  show (Temp t) = "t" ++ show t
  show (RegOperand r) = "reg " ++ show r
  show (StackOperand n) = "stack " ++ show n

data Terminator
  = Return {retVal :: Maybe Operand}
  | Jump {target :: BlockId}
  | CondJump {cond :: Operand, trueBlockId :: BlockId, falseBlockId :: BlockId}
  deriving (Eq)

instance Show Terminator where
  show (Return Nothing) = "return"
  show (Return (Just t)) = "return " ++ show t
  show (Jump target) = "goto " ++ target
  show (CondJump cond trueBlockId falseBlockId) =
    "if " ++ show cond ++ " goto " ++ trueBlockId ++ " else goto " ++ falseBlockId

data Inst
  = Assign {dst :: Operand, src :: Operand}
  | UnaryOp {dst :: Operand, unop :: UnaryOp, src :: Operand}
  | BinOp {dst :: Operand, binop :: BinOp, left :: Operand, right :: Operand}
  | Load {dst :: Operand, srcVar :: Var}
  | Store {dstVar :: Var, src :: Operand}
  | Call {ret :: Maybe Operand, funId :: Id, argCount :: Int}
  | Param {param :: Operand}
  deriving (Eq)

instance Show Inst where
  show Assign {dst, src} = show dst ++ " = " ++ show src
  show UnaryOp {dst, unop, src} = show dst ++ " = " ++ show unop ++ show src
  show BinOp {dst, binop, left, right} =
    show dst ++ " = " ++ show left ++ " " ++ show binop ++ " " ++ show right
  show Load {dst, srcVar} = show dst ++ " = " ++ show srcVar
  show Store {dstVar, src} = show dstVar ++ " = " ++ show src
  show Call {ret = Just t, funId, argCount} = show t ++ " = call " ++ funId ++ ", " ++ show argCount
  show Call {ret = Nothing, funId, argCount} = "call " ++ funId ++ ", " ++ show argCount
  show Param {param} = "param " ++ show param

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

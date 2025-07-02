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
  = Return {retOperand :: Maybe Operand}
  | Jump {jumpTarget :: BlockId}
  | CondJump {condOperand :: Operand, condTrueBlockId :: BlockId, condFalseBlockId :: BlockId}
  deriving (Eq)

instance Show Terminator where
  show (Return Nothing) = "return"
  show (Return (Just operand)) = "return " ++ show operand
  show (Jump target) = "goto " ++ target
  show (CondJump cond trueBlockId falseBlockId) =
    "if " ++ show cond ++ " goto " ++ trueBlockId ++ " else goto " ++ falseBlockId

data Inst
  = Assign {instDst :: Operand, instSrc :: Operand}
  | UnaryOp {instDst :: Operand, instUnop :: UnaryOp, instSrc :: Operand}
  | BinOp {instDst :: Operand, instBinop :: BinOp, instLeft :: Operand, instRight :: Operand}
  | Call {callRet :: Maybe Operand, callFunId :: Id, callArgCount :: Int}
  | Param {paramOperand :: Operand}
  deriving (Eq)

instance Show Inst where
  show Assign {instDst, instSrc} = show instDst ++ " = " ++ show instSrc
  show UnaryOp {instDst, instUnop, instSrc} = show instDst ++ " = " ++ show instUnop ++ show instSrc
  show BinOp {instDst, instBinop, instLeft, instRight} =
    show instDst ++ " = " ++ show instLeft ++ " " ++ show instBinop ++ " " ++ show instRight
  show Call {callRet = Just ret, callFunId, callArgCount} = show ret ++ " = call " ++ callFunId ++ ", " ++ show callArgCount
  show Call {callRet = Nothing, callFunId, callArgCount} = "call " ++ callFunId ++ ", " ++ show callArgCount
  show Param {paramOperand} = "param " ++ show paramOperand

-- A Basic Block is a sequence of instructions that starts with a blockId
-- and ends with a control flow instruction (Jump, CondJump, Return).
-- For simplicity here, we'll just list instructions and assume the last one is control flow.
-- A more rigorous CFG would explicitly link blocks.
data BasicBlock = BasicBlock
  { cfgBlockId :: BlockId,
    blockInsts :: [Inst],
    blockTerminator :: Terminator
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
  { cfgEntryBlockId :: BlockId,
    cfgExitBlocks :: Set BlockId,
    cfgBlocks :: [BasicBlock]
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
  { funId :: Id,
    funArgs :: [Env.Symbol],
    funLocals :: [Env.Symbol],
    funCfg :: CFG
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
  { programFuns :: [Fun],
    programExternFuns :: [ExternFun],
    programMainFun :: Maybe Fun
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

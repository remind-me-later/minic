module Mir.Types
  ( Temp (..),
    incTempLabel,
    BasicBlockId,
    Register (..),
    availableRegisters,
    Operand (..),
    Terminator (..),
    Inst (..),
    BasicBlock (..),
    CFG (..),
    Fun (..),
    ExternFun (..),
    Program (..),
  )
where

import Data.Set
import SymbolTable qualified
import TypeSystem

newtype Temp
  = Temp {tempLabel :: Int}
  deriving (Eq, Ord)

incTempLabel :: Temp -> Temp
incTempLabel (Temp label) = Temp (label + 1)

instance Show Temp where
  show (Temp label) = "t" ++ show label

type BasicBlockId = String

data Register = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
  deriving (Eq, Ord, Show, Enum, Bounded)

availableRegisters :: [Register]
availableRegisters = [R1 .. R8]

data Operand
  = ConstInt Int
  | ConstChar Char
  | TempOperand Temp
  | RegOperand Register
  | StackOperand Int
  deriving (Eq)

instance Show Operand where
  show (ConstInt n) = "const " ++ show n
  show (ConstChar c) = "const " ++ show c
  show (TempOperand t) = show t
  show (RegOperand r) = "reg " ++ show r
  show (StackOperand n) = "stack " ++ show n

data Terminator
  = Return {retOperand :: Maybe Operand}
  | Jump {jumpTarget :: BasicBlockId}
  | CondJump {condOperand :: Operand, condTrueBasicBlockId :: BasicBlockId, condFalseBasicBlockId :: BasicBlockId}
  deriving (Eq)

instance Show Terminator where
  show (Return Nothing) = "return"
  show (Return (Just operand)) = "return " ++ show operand
  show (Jump target) = "goto " ++ target
  show (CondJump cond trueBasicBlockId falseBasicBlockId) =
    "if " ++ show cond ++ " goto " ++ trueBasicBlockId ++ " else goto " ++ falseBasicBlockId

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
data BasicBlock = BasicBlock
  { cfgBasicBlockId :: BasicBlockId,
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
  { cfgEntryBasicBlockId :: BasicBlockId,
    cfgExitBlocks :: Set BasicBlockId,
    -- TODO: this list should be non empty
    cfgBlocks :: [BasicBlock]
  }
  deriving (Eq)

instance Show CFG where
  show (CFG entryBasicBlockId exitBlocks blocks) =
    "CFG:\n"
      ++ "Entry Block: "
      ++ entryBasicBlockId
      ++ "\n"
      ++ "Exit Blocks: "
      ++ unwords (show <$> toList exitBlocks)
      ++ "\n"
      ++ "Blocks:\n"
      ++ unlines (show <$> blocks)

data Fun = Fun
  { funId :: Id,
    funArgs :: [SymbolTable.Symbol],
    funLocals :: [SymbolTable.Symbol],
    funCfg :: CFG
  }
  deriving (Eq)

instance Show Fun where
  show (Fun identifier args locals cfg) =
    "Function: "
      ++ identifier
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
  show (ExternFun identifier) = "Extern Function: " ++ identifier

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

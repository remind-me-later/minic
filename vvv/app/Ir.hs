-- Based on the Dragon Book's TAC
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Ir
  ( Temp,
    Label,
    MirOperand (..),
    MirInstr (..),
    MirBasicBlock (..),
    MirFunction (..),
    MirProgram (..),
    programToMir,
  )
where

import Ast qualified (Block (..), Exp (..), ExpInner (..), Fun (..), Ident, Operator, Program (..), Stmt (..), Ty (..), VarDef (..))
import TypeCheck (TypedBlock, TypedExp, TypedFun, TypedProgram, TypedStmt)

-- A temporary variable or register
type Temp = Int

-- A label for a basic block
type Label = String

-- Operands for instructions
data MirOperand
  = ConstInt Int
  | Temp Temp -- Value held in a temporary
  | Var Ast.Ident -- Represents a named variable from the source language
  deriving (Eq)

instance Show MirOperand where
  show (ConstInt n) = "const " ++ show n
  show (Temp t) = "t" ++ show t
  show (Var id) = "var " ++ id

-- Three-Address Code Instructions
data MirInstr
  = -- Assignment and Operations
    Assign Temp MirOperand -- t1 = operand
  | BinOp Temp Ast.Operator Temp Temp -- t1 = op1 operator op2
  | -- Memory / Variable Access (can be refined if stack/heap is more explicit)
    Load Temp Ast.Ident -- t1 = variable_name (load from source variable)
  | Store Ast.Ident Temp -- variable_name = operand (store to source variable)
  | -- Function Calls
    Call (Maybe Temp) Ast.Ident Int
  | Param Temp -- Pass an operand as a parameter to a function
  | Return (Maybe Temp) -- Return from function, with optional value
  | -- Control Flow
    DefLabel Label -- Defines a label: L1:
  | Jump Label -- goto L1
  | CondJump Temp Label Label -- if operand != 0 then goto L_true else goto L_false
  deriving (Eq)

instance Show MirInstr where
  show (Assign t op) = "t" ++ show t ++ " = " ++ show op
  show (BinOp t t' a b) = "t" ++ show t ++ " = " ++ show a ++ " " ++ show t' ++ " " ++ show b
  show (Load t id) = "t" ++ show t ++ " = " ++ "var " ++ id
  show (Store id t) = "var " ++ id ++ " = " ++ "t" ++ show t
  show (Call (Just t) id n) = "t" ++ show t ++ " = call " ++ id ++ ", " ++ show n
  show (Call Nothing id n) = "call " ++ id ++ ", " ++ show n
  show (Param t) = "param " ++ "t" ++ show t
  show (Return (Just t)) = "return " ++ "t" ++ show t
  show (Return Nothing) = "return"
  show (DefLabel label) = label ++ ":"
  show (Jump label) = "goto " ++ label
  show (CondJump t trueLabel falseLabel) =
    "if " ++ show t ++ " then goto " ++ trueLabel ++ " else goto " ++ falseLabel

-- A Basic Block is a sequence of instructions that starts with a label
-- and ends with a control flow instruction (Jump, CondJump, Return).
-- For simplicity here, we'll just list instructions and assume the last one is control flow.
-- A more rigorous CFG would explicitly link blocks.
data MirBasicBlock = MirBasicBlock
  { label :: Label,
    instrs :: [MirInstr]
  }
  deriving (Eq)

instance Show MirBasicBlock where
  show (MirBasicBlock label instructions) =
    label ++ ":\n" ++ unlines (map (("  " ++) . show) instructions)

-- An IR representation of a function
data MirFunction = IrFunction
  { irFunName :: Ast.Ident,
    irFunArgs :: [Ast.Ident], -- Names of arguments, can be mapped to Temps
    irFunEntryLabel :: Label,
    irFunBlocks :: [MirBasicBlock] -- List of basic blocks in the function
    -- Could also be a Map Label MirBasicBlock for easier lookup
  }
  deriving (Eq)

instance Show MirFunction where
  show (IrFunction name args entryLabel blocks) =
    "Function: "
      ++ name
      ++ "\n"
      ++ "Args: "
      ++ unwords args
      ++ "\n"
      ++ "Entry: "
      ++ entryLabel
      ++ "\n"
      ++ "Blocks:\n"
      ++ unlines (map show blocks)

-- The entire program in IR
newtype MirProgram = IrProgram
  { irFunctions :: [MirFunction]
  -- Potentially global variable declarations if supported
  }
  deriving (Eq)

instance Show MirProgram where
  show (IrProgram functions) = unlines (map show functions)

newTemp :: Temp -> Temp
newTemp current = current + 1

expToMir :: TypedExp -> [MirInstr] -> Temp -> ([MirInstr], Temp)
expToMir Ast.Exp {annot, exp} acc t
  | Ast.IdentifierExp {id} <- exp =
      (acc ++ [Load t id], t)
  | Ast.NumberExp {num} <- exp =
      (acc ++ [Assign t (ConstInt num)], t)
  | Ast.BinExp {left, op, right} <- exp = do
      let (leftAcc, lt) = expToMir left acc t
      let (rightAcc, rt) = expToMir right leftAcc (newTemp lt)
      let binOpTemp = newTemp rt
      (rightAcc ++ [BinOp binOpTemp op lt rt], binOpTemp)
  | Ast.Call {id, args} <- exp = do
      let (argAcc, argTemps) =
            foldl
              ( \(acc', temps) arg ->
                  let (newAcc, nt) = expToMir arg acc' (last temps) in (newAcc, temps ++ [nt])
              )
              (acc, [t])
              args
      case annot of
        Ast.Void ->
          let argtmps = tail argTemps
           in (argAcc ++ map Param argtmps ++ [Call Nothing id (length argtmps)], last argTemps)
        _ ->
          let argtmps = tail argTemps
              callTemp = newTemp (last argTemps)
           in (argAcc ++ map Param argtmps ++ [Call (Just callTemp) id (length argtmps)], callTemp)

stmtToMir :: TypedStmt -> [MirInstr] -> Temp -> ([MirInstr], Temp)
stmtToMir stmt acc t
  | Ast.ExpStmt {exp} <- stmt =
      expToMir exp acc t
  | Ast.LetStmt {vardef = Ast.VarDef {id}, exp} <- stmt = do
      let (newAcc, nt) = expToMir exp acc t
      (newAcc ++ [Store id nt], newTemp nt)
  | Ast.AssignStmt {id, exp} <- stmt = do
      let (newAcc, nt) = expToMir exp acc t
      (newAcc ++ [Store id nt], newTemp nt)
  | Ast.ReturnStmt {exp} <- stmt = do
      let (newAcc, nt) = expToMir exp acc t
      (newAcc ++ [Return (Just nt)], newTemp nt)
  | Ast.IfStmt {cond, ifBody, elseBody} <- stmt = do
      let condTemp = t
      let ifLabel = "if_true"
      let elseLabel = "if_false"
      let endLabel = "if_end"
      let (newAcc, nt) = expToMir cond (acc ++ [CondJump condTemp ifLabel elseLabel]) condTemp
      let ifBlock = MirBasicBlock {label = ifLabel, instrs = blockToMir ifBody}
      let elseBlock = case elseBody of
            Just body -> MirBasicBlock {label = elseLabel, instrs = blockToMir body}
            Nothing -> MirBasicBlock {label = elseLabel, instrs = []}
      let endBlock = MirBasicBlock {label = endLabel, instrs = [Jump endLabel]}
      ( newAcc
          ++ [DefLabel ifLabel]
          ++ ifBlock.instrs
          ++ [DefLabel elseLabel]
          ++ elseBlock.instrs
          ++ [DefLabel endLabel]
          ++ endBlock.instrs,
        nt
        )
  | Ast.WhileStmt {cond, body} <- stmt = do
      let condLabel = "while_cond"
      let loopLabel = "while_loop"
      let endLabel = "while_end"
      let (condInstrs, nt) = expToMir cond acc t
      let loopBlock = MirBasicBlock {label = loopLabel, instrs = blockToMir body}
      let endBlock = MirBasicBlock {label = endLabel, instrs = []}
      ( [DefLabel "whileBegin"]
          ++ condInstrs
          ++ [DefLabel condLabel]
          ++ [CondJump nt loopLabel endLabel]
          ++ [DefLabel loopLabel]
          ++ loopBlock.instrs
          ++ [Jump condLabel]
          ++ [DefLabel endLabel]
          ++ endBlock.instrs,
        newTemp nt
        )

blockToMir :: TypedBlock -> [MirInstr]
blockToMir Ast.Block {stmts} = do
  let initialTemp = 0
  let acc = []
  let f =
        foldl
          (\(instrs, temp) stmt -> stmtToMir stmt instrs temp)
          (acc, initialTemp)
          stmts
  fst f -- Return only the instructions, not the final temp

funToMir :: TypedFun -> [MirFunction] -> [MirFunction]
funToMir Ast.Fun {id, args, body} acc =
  let funName = id
      funArgs = map (\Ast.VarDef {id} -> id) args
      entryLabel = funName ++ "_entry"
      blocks = [MirBasicBlock {label = entryLabel, instrs = blockToMir body}]
   in acc ++ [IrFunction {irFunName = funName, irFunArgs = funArgs, irFunEntryLabel = entryLabel, irFunBlocks = blocks}]

programToMir :: TypedProgram -> MirProgram
programToMir Ast.Program {funcs} =
  let irFunctions = foldl (flip funToMir) [] funcs
   in IrProgram {irFunctions = irFunctions}

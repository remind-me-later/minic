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

import Ast qualified (Block (..), Expr (..), Fun (..), Ident, Operator, Program (..), Stmt (..), Ty (..), VarDef (..))
import TypeCheck (TypedBlock, TypedExpr, TypedFun, TypedProgram, TypedStmt)

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
    Call (Maybe Temp) Ast.Ident
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
  show (Call (Just t) id) = "call " ++ id ++ " -> " ++ "t" ++ show t
  show (Call Nothing id) = "call " ++ id
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
data MirBasicBlock = BasicBlock
  { bbLabel :: Label,
    bbInstructions :: [MirInstr]
  }
  deriving (Eq)

instance Show MirBasicBlock where
  show (BasicBlock label instructions) =
    label ++ ":\n" ++ unlines (map (("  " ++) . show) instructions)

-- An IR representation of a function
data MirFunction = IrFunction
  { irFunName :: Ast.Ident,
    irFunArgs :: [Ast.Ident], -- Names of arguments, can be mapped to Temps
    irFunEntryLabel :: Label,
    irFunBlocks :: [MirBasicBlock] -- List of basic blocks in the function
    -- Could also be a Map Label BasicBlock for easier lookup
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

exprToMir :: TypedExpr -> [MirInstr] -> Temp -> ([MirInstr], Temp)
exprToMir expr acc t
  | Ast.IdentifierExpr {id} <- expr =
      (acc ++ [Load t id], t)
  | Ast.NumberExpr {num} <- expr =
      (acc ++ [Assign t (ConstInt num)], t)
  | Ast.BinExpr {left, op, right} <- expr = do
      let (leftAcc, lt) = exprToMir left acc t
      let (rightAcc, rt) = exprToMir right leftAcc (newTemp lt)
      let binOpTemp = newTemp rt
      (rightAcc ++ [BinOp binOpTemp op lt rt], binOpTemp)
  | Ast.Call {id, args, annot} <- expr = do
      let (argAcc, argTemps) =
            foldl
              ( \(acc', temps) arg ->
                  let (newAcc, nt) = exprToMir arg acc' (last temps) in (newAcc, temps ++ [nt])
              )
              (acc, [t])
              args
      case annot of
        Ast.Void ->
          (argAcc ++ map Param (tail argTemps) ++ [Call Nothing id], last argTemps)
        _ ->
          let callTemp = newTemp (last argTemps)
           in (argAcc ++ map Param (tail argTemps) ++ [Call (Just callTemp) id], callTemp)

stmtToMir :: TypedStmt -> [MirInstr] -> Temp -> ([MirInstr], Temp)
stmtToMir stmt acc t
  | Ast.ExprStmt {expr} <- stmt =
      exprToMir expr acc t
  | Ast.LetStmt {vardef = Ast.VarDef {id}, expr} <- stmt = do
      let (newAcc, nt) = exprToMir expr acc t
      (newAcc ++ [Store id nt], newTemp nt)
  | Ast.AssignStmt {id, expr} <- stmt = do
      let (newAcc, nt) = exprToMir expr acc t
      (newAcc ++ [Store id nt], newTemp nt)
  | Ast.ReturnStmt {expr} <- stmt = do
      let (newAcc, nt) = exprToMir expr acc t
      (newAcc ++ [Return (Just nt)], newTemp nt)
  | Ast.IfStmt {cond, ifBody, elseBody} <- stmt = do
      let condTemp = t
      let ifLabel = "if_true"
      let elseLabel = "if_false"
      let endLabel = "if_end"
      let (newAcc, nt) = exprToMir cond (acc ++ [CondJump condTemp ifLabel elseLabel]) condTemp
      let ifBlock = BasicBlock {bbLabel = ifLabel, bbInstructions = blockToMir ifBody []}
      let elseBlock = case elseBody of
            Just body -> BasicBlock {bbLabel = elseLabel, bbInstructions = blockToMir body []}
            Nothing -> BasicBlock {bbLabel = elseLabel, bbInstructions = []}
      let endBlock = BasicBlock {bbLabel = endLabel, bbInstructions = [Jump endLabel]}
      ( newAcc
          ++ [DefLabel ifLabel]
          ++ ifBlock.bbInstructions
          ++ [DefLabel elseLabel]
          ++ elseBlock.bbInstructions
          ++ [DefLabel endLabel]
          ++ endBlock.bbInstructions,
        nt
        )
  | Ast.WhileStmt {cond, body} <- stmt = do
      let condLabel = "while_cond"
      let loopLabel = "while_loop"
      let endLabel = "while_end"
      let (condInstrs, nt) = exprToMir cond acc t
      let loopBlock = BasicBlock {bbLabel = loopLabel, bbInstructions = blockToMir body []}
      let endBlock = BasicBlock {bbLabel = endLabel, bbInstructions = []}
      ( [DefLabel "whileBegin"]
          ++ condInstrs
          ++ [DefLabel condLabel]
          ++ [CondJump nt loopLabel endLabel]
          ++ [DefLabel loopLabel]
          ++ loopBlock.bbInstructions
          ++ [Jump condLabel]
          ++ [DefLabel endLabel]
          ++ endBlock.bbInstructions,
        newTemp nt
        )

blockToMir :: TypedBlock -> [MirInstr] -> [MirInstr]
blockToMir Ast.Block {stmts} acc = do
  let initialTemp = 0
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
      blocks = [BasicBlock {bbLabel = entryLabel, bbInstructions = blockToMir body []}]
   in acc ++ [IrFunction {irFunName = funName, irFunArgs = funArgs, irFunEntryLabel = entryLabel, irFunBlocks = blocks}]

programToMir :: TypedProgram -> MirProgram
programToMir Ast.Program {funcs} =
  let irFunctions = foldl (flip funToMir) [] funcs
   in IrProgram {irFunctions = irFunctions}

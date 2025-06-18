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

import Ast qualified
  ( Block (..),
    Exp (..),
    ExpInner (..),
    Fun (..),
    Id,
    Operator,
    Program (..),
    Stmt (..),
    Ty (..),
    VarDef (..),
  )
import Control.Monad (foldM)
import Control.Monad.State (State, get, modify, runState)
import Scope qualified
import TypeCheck
  ( TypedBlock,
    TypedExp,
    TypedFun,
    TypedProgram,
    TypedStmt,
  )

-- A temporary variable or register
type Temp = Int

-- A label for a basic block
type Label = String

data MirVar
  = Local Ast.Id -- Local variable in a function
  | Arg Ast.Id -- Argument to a function
  deriving (Eq)

instance Show MirVar where
  show (Local id) = "local " ++ id
  show (Arg id) = "arg " ++ id

-- Operands for instructions
data MirOperand
  = ConstInt Int
  | Temp Temp -- Value held in a temporary
  | Var MirVar -- Represents a named variable from the source language
  deriving (Eq)

instance Show MirOperand where
  show (ConstInt n) = "const " ++ show n
  show (Temp t) = "t" ++ show t
  show (Var (Local id)) = "local " ++ id
  show (Var (Arg id)) = "arg " ++ id

-- Three-Address Code Instructions
data MirInstr
  = -- Assignment and Operations
    Assign Temp MirOperand -- t1 = operand
  | BinOp Temp Ast.Operator Temp Temp -- t1 = op1 operator op2
  | -- Memory / Variable Access (can be refined if stack/heap is more explicit)
    Load Temp MirVar -- t1 = variable_name (load from source variable)
  | Store MirVar Temp -- variable_name = operand (store to source variable)
  | -- Function Calls
    Call (Maybe Temp) Ast.Id Int
  | Param Temp -- Pass an operand as a parameter to a function
  | Return (Maybe Temp) -- Return from function, with optional value
  | -- Control Flow
    DefLabel Label -- Defines a label: L1:
  | Jump Label -- goto L1
  | CondJump Temp Label Label -- if operand != 0 then goto L_true else goto L_false
  deriving (Eq)

instance Show MirInstr where
  show (Assign t op) = "t" ++ show t ++ " = " ++ show op
  show (BinOp t1 op t2 t3) = "t" ++ show t1 ++ " = " ++ "t" ++ show t2 ++ " " ++ show op ++ " " ++ "t" ++ show t3
  show (Load t v) = "t" ++ show t ++ " = " ++ show v
  show (Store v t) = show v ++ " = t" ++ show t
  show (Call (Just t) id n) = "t" ++ show t ++ " = call " ++ id ++ ", " ++ show n
  show (Call Nothing id n) = "call " ++ id ++ ", " ++ show n
  show (Param t) = "param " ++ "t" ++ show t
  show (Return (Just t)) = "return " ++ "t" ++ show t
  show (Return Nothing) = "return"
  show (DefLabel label) = label ++ ":"
  show (Jump label) = "goto " ++ label
  show (CondJump t trueLabel falseLabel) =
    "if " ++ "t" ++ show t ++ " then goto " ++ trueLabel ++ " else goto " ++ falseLabel

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
data MirFunction = MirFunction
  { irFunName :: Ast.Id,
    irFunArgs :: [Ast.Id], -- Names of arguments, can be mapped to Temps
    irFunEntryLabel :: Label,
    irFunBlocks :: [MirBasicBlock] -- List of basic blocks in the function
    -- Could also be a Map Label MirBasicBlock for easier lookup
  }
  deriving (Eq)

instance Show MirFunction where
  show (MirFunction name args entryLabel blocks) =
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

data TranslationState = TranslationState
  { currentTemp :: Temp, -- Current temporary variable counter
    labelCount :: Int -- Current label count for generating unique labels
  }

incTemp :: TranslationState -> TranslationState
incTemp (TranslationState t lc) = TranslationState {currentTemp = t + 1, labelCount = lc}

incLabel :: TranslationState -> TranslationState
incLabel (TranslationState t lc) = TranslationState {currentTemp = t, labelCount = lc + 1}

transexp :: Scope.Scope -> TypedExp -> State TranslationState [MirInstr]
transexp scope Ast.Exp {annot, exp}
  | Ast.IdifierExp {id} <- exp =
      case Scope.lookup id scope of
        Just Scope.Symbol {ty = Ast.FunTy {}} ->
          error $ "Cannot use function " ++ id ++ " as variable"
        Just Scope.Symbol {variant} ->
          case variant of
            Scope.Local -> do
              TranslationState {currentTemp = t} <- get
              return [Load t (Local id)]
            Scope.Argument -> do
              TranslationState {currentTemp = t} <- get
              return [Load t (Arg id)]
            _ -> error $ "Unexpected symbol variant for variable: " ++ show variant
        Nothing -> error $ "Undefined variable: " ++ id
  | Ast.NumberExp {num} <- exp = do
      TranslationState {currentTemp = t} <- get
      return [Assign t (ConstInt num)]
  | Ast.BinExp {left, op, right} <- exp = do
      linst <- transexp scope left
      TranslationState {currentTemp = lt} <- get
      modify incTemp
      rinst <- transexp scope right
      TranslationState {currentTemp = rt} <- get
      modify incTemp
      TranslationState {currentTemp = t'} <- get
      return (linst ++ rinst ++ [BinOp t' op lt rt])
  | Ast.Call {id, args} <- exp = do
      argInsts <-
        foldM
          ( \acc arg -> do
              insts <- transexp scope arg
              TranslationState {currentTemp = t} <- get
              modify incTemp
              return (acc ++ insts ++ [Param t])
          )
          []
          args
      case annot of
        Ast.VoidTy -> do
          return (argInsts ++ [Call Nothing id (length args)])
        _ -> do
          TranslationState {currentTemp = t} <- get
          modify incTemp
          return (argInsts ++ [Call (Just t) id (length args)])

stmtToMir :: Scope.Scope -> TypedStmt -> State TranslationState [MirInstr]
stmtToMir scope stmt
  | Ast.ExpStmt {exp} <- stmt = transexp scope exp
  | Ast.LetStmt {vardef = Ast.VarDef {id}, exp} <- stmt = do
      insts <- transexp scope exp
      TranslationState {currentTemp = t} <- get
      modify incTemp
      return $ insts ++ [Store (Local id) t]
  | Ast.AssignStmt {id, exp} <- stmt = do
      insts <- transexp scope exp
      case Scope.lookup id scope of
        Just Scope.Symbol {variant = Scope.Local} -> do
          TranslationState {currentTemp = t} <- get
          modify incTemp
          return $ insts ++ [Store (Local id) t]
        Just Scope.Symbol {variant = Scope.Argument} -> do
          TranslationState {currentTemp = t} <- get
          modify incTemp
          return $ insts ++ [Store (Arg id) t]
        _ -> error $ "Undefined variable: " ++ id
  | Ast.ReturnStmt {retexp} <- stmt = do
      case retexp of
        Just exp -> do
          insts <- transexp scope exp
          TranslationState {currentTemp = t} <- get
          modify incTemp
          return $ insts ++ [Return (Just t)]
        Nothing -> do
          return [Return Nothing]
  -- \| Ast.IfStmt {cond, ifBody, elseBody} <- stmt = do
  --     let condTemp = t
  --     let ifLabel = "if_true_" ++ show l.current
  --     let l' = incLabelCount l
  --     let elseLabel = "if_false_" ++ show l'.current
  --     let l'' = incLabelCount l'
  --     let endLabel = "if_end_" ++ show l''.current
  --     let (newAcc, nt) = transexp scope cond (acc ++ [CondJump condTemp ifLabel elseLabel]) condTemp
  --     let ifBlock = MirBasicBlock {label = ifLabel, instrs = blockToMir ifBody}
  --     let elseBlock = case elseBody of
  --           Just body -> MirBasicBlock {label = elseLabel, instrs = blockToMir body}
  --           Nothing -> MirBasicBlock {label = elseLabel, instrs = []}
  --     let endBlock = MirBasicBlock {label = endLabel, instrs = [Jump endLabel]}
  --     ( ( newAcc
  --           ++ [DefLabel ifLabel]
  --           ++ ifBlock.instrs
  --           ++ [DefLabel elseLabel]
  --           ++ elseBlock.instrs
  --           ++ [DefLabel endLabel]
  --           ++ endBlock.instrs,
  --         nt
  --       ),
  --       incLabelCount l''
  --       )
  | Ast.WhileStmt {cond, body} <- stmt = do
      TranslationState {labelCount = l} <- get
      modify incLabel
      let condLabel = "while_cond_" ++ show l
      TranslationState {labelCount = l} <- get
      modify incLabel
      let loopLabel = "while_loop_" ++ show l
      TranslationState {labelCount = l} <- get
      modify incLabel
      let endLabel = "while_end_" ++ show l
      modify incLabel
      condInstrs <- transexp scope cond
      TranslationState {currentTemp = t} <- get
      let loopBlock = MirBasicBlock {label = loopLabel, instrs = blockToMir body}
      let endBlock = MirBasicBlock {label = endLabel, instrs = []}
      return
        ( condInstrs
            ++ [DefLabel condLabel]
            ++ [CondJump t loopLabel endLabel]
            ++ [DefLabel loopLabel]
            ++ loopBlock.instrs
            ++ [Jump condLabel]
            ++ [DefLabel endLabel]
            ++ endBlock.instrs
        )
  | _ <- stmt = do
      error $ "Unsupported statement: " ++ show stmt

blockToMir :: TypedBlock -> [MirInstr]
blockToMir Ast.Block {annot, stmts} = do
  let initialState = TranslationState {currentTemp = 0, labelCount = 0}
  let (mirInstrs, _) =
        runState
          ( foldM
              ( \acc stmt -> do
                  insts <- stmtToMir annot stmt
                  return (acc ++ insts)
              )
              []
              stmts
          )
          initialState
  mirInstrs

funToMir :: TypedFun -> [MirFunction] -> [MirFunction]
funToMir Ast.Fun {id, args, body} acc =
  let funName = id
      funArgs = map (\Ast.VarDef {id} -> id) args
      entryLabel = funName ++ "_entry"
      blocks = [MirBasicBlock {label = entryLabel, instrs = blockToMir body}]
   in acc
        ++ [ MirFunction
               { irFunName = funName,
                 irFunArgs = funArgs,
                 irFunEntryLabel = entryLabel,
                 irFunBlocks = blocks
               }
           ]

programToMir :: TypedProgram -> MirProgram
programToMir Ast.Program {funcs} =
  let irFunctions = foldl (flip funToMir) [] funcs
   in IrProgram {irFunctions = irFunctions}

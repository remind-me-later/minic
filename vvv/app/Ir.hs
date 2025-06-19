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
    transProgram,
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
import Control.Applicative ((<|>))
import Control.Monad (foldM, foldM_)
import Control.Monad.State (State, get, modify, runState)
import Control.Monad.State.Class (gets)
import Env qualified
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
    Jump Label -- goto L1
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
  { currentTemp :: Temp,
    labelCount :: Int,
    blocks :: [MirBasicBlock],
    envStack :: [Env.Env]
  }

incTemp :: TranslationState -> TranslationState
incTemp ts@TranslationState {currentTemp} = ts {currentTemp = currentTemp + 1}

incLabel :: TranslationState -> TranslationState
incLabel ts@TranslationState {labelCount} = ts {labelCount = labelCount + 1}

addInstsToBlock :: [MirInstr] -> TranslationState -> TranslationState
addInstsToBlock insts ts@TranslationState {blocks = curBlock : restBlocks} =
  ts {blocks = curBlock {instrs = curBlock.instrs ++ insts} : restBlocks}
addInstsToBlock _ TranslationState {blocks = []} =
  error "No current block to add instructions to"

openBlock :: String -> TranslationState -> TranslationState
openBlock label ts@TranslationState {blocks} =
  ts {blocks = MirBasicBlock {label = label, instrs = []} : blocks}

stackEnv :: Env.Env -> TranslationState -> TranslationState
stackEnv env ts@TranslationState {envStack} =
  ts {envStack = env : envStack}

popEnv :: TranslationState -> TranslationState
popEnv ts@TranslationState {envStack} =
  case envStack of
    [] -> error "No environment to pop"
    (_ : rest) -> ts {envStack = rest}

lookupId :: Ast.Id -> TranslationState -> Maybe Env.Symbol
lookupId id ts@TranslationState {envStack} =
  case envStack of
    [] -> Nothing
    (env : rest) -> Env.lookup id env <|> lookupId id (ts {envStack = rest})

transExp :: TypedExp -> State TranslationState [MirInstr]
transExp Ast.Exp {annot, exp}
  | Ast.IdExp {id} <- exp = do
      symb <- gets (lookupId id)
      case symb of
        Just Env.Symbol {ty = Ast.FunTy {}} ->
          error $ "Cannot use function " ++ id ++ " as variable"
        Just Env.Symbol {variant} ->
          case variant of
            Env.Local -> do
              TranslationState {currentTemp = t} <- get
              return [Load t (Local id)]
            Env.Argument -> do
              TranslationState {currentTemp = t} <- get
              return [Load t (Arg id)]
            _ -> error $ "Unexpected symbol variant for variable: " ++ show variant
        Nothing -> error $ "Undefined variable: " ++ id
  | Ast.NumberExp {num} <- exp = do
      TranslationState {currentTemp = t} <- get
      return [Assign t (ConstInt num)]
  | Ast.BinExp {left, op, right} <- exp = do
      linst <- transExp left
      TranslationState {currentTemp = lt} <- get
      modify incTemp
      rinst <- transExp right
      TranslationState {currentTemp = rt} <- get
      modify incTemp
      TranslationState {currentTemp = t'} <- get
      return (linst ++ rinst ++ [BinOp t' op lt rt])
  | Ast.Call {id, args} <- exp = do
      argInsts <-
        foldM
          ( \acc arg -> do
              insts <- transExp arg
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

transStmt :: TypedStmt -> State TranslationState ()
transStmt stmt
  | Ast.ExpStmt {exp} <- stmt = do
      insts <- transExp exp
      modify (addInstsToBlock insts)
  | Ast.LetStmt {vardef = Ast.VarDef {id}, exp} <- stmt = do
      insts <- transExp exp
      TranslationState {currentTemp = t} <- get
      modify incTemp
      modify (addInstsToBlock (insts ++ [Store (Local id) t]))
  | Ast.AssignStmt {id, exp} <- stmt = do
      insts <- transExp exp
      symb <- gets (lookupId id)
      case symb of
        Just Env.Symbol {variant = Env.Local} -> do
          TranslationState {currentTemp = t} <- get
          modify incTemp
          -- return $ insts ++ [Store (Local id) t]
          modify (addInstsToBlock (insts ++ [Store (Local id) t]))
        Just Env.Symbol {variant = Env.Argument} -> do
          TranslationState {currentTemp = t} <- get
          modify incTemp
          -- return $ insts ++ [Store (Arg id) t]
          modify (addInstsToBlock (insts ++ [Store (Arg id) t]))
        _ -> error $ "Undefined variable: " ++ id
  | Ast.ReturnStmt {retexp} <- stmt = do
      case retexp of
        Just exp -> do
          insts <- transExp exp
          TranslationState {currentTemp = t} <- get
          modify incTemp
          -- return $ insts ++ [Return (Just t)]
          modify (addInstsToBlock (insts ++ [Return (Just t)]))
        Nothing -> do
          -- return [Return Nothing]
          modify (addInstsToBlock [Return Nothing])
  | Ast.IfStmt {cond, ifBody, elseBody} <- stmt = do
      TranslationState {labelCount = l} <- get
      modify incLabel
      let thenLabel = "if_then_" ++ show l
      condInstrs <- transExp cond

      case elseBody of
        Just elseBody -> do
          TranslationState {labelCount = l} <- get
          modify incLabel
          let elseLabel = "if_else_" ++ show l
          TranslationState {labelCount = l} <- get
          modify incLabel
          let endLabel = "if_end_" ++ show l

          TranslationState {currentTemp = t} <- get
          modify (addInstsToBlock $ condInstrs ++ [CondJump t thenLabel elseLabel])
          _ <- transBlock thenLabel ifBody
          modify (addInstsToBlock [Jump endLabel])
          _ <- transBlock elseLabel elseBody
          modify (addInstsToBlock [Jump endLabel])
          modify (openBlock endLabel)
        Nothing -> do
          TranslationState {labelCount = l} <- get
          modify incLabel
          let endLabel = "if_end_" ++ show l

          TranslationState {currentTemp = t} <- get
          modify (addInstsToBlock $ condInstrs ++ [CondJump t thenLabel endLabel])
          _ <- transBlock thenLabel ifBody
          modify (addInstsToBlock [Jump endLabel])
          modify (openBlock endLabel)
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
      condInstrs <- transExp cond
      modify (openBlock condLabel)
      TranslationState {currentTemp = t} <- get
      modify (addInstsToBlock $ condInstrs ++ [CondJump t loopLabel endLabel])
      _ <- transBlock loopLabel body
      modify (addInstsToBlock [Jump condLabel])
      modify (openBlock endLabel)

transBlock :: String -> TypedBlock -> State TranslationState ()
transBlock label Ast.Block {annot = scope, stmts} = do
  modify (openBlock label)
  modify (stackEnv scope)
  foldM_ (\_ stmt -> transStmt stmt) () (reverse stmts)
  modify popEnv

transFun :: TypedFun -> MirFunction
transFun Ast.Fun {id, args, body} =
  let funName = id
      funArgs = map (\Ast.VarDef {id} -> id) args
      entryLabel = funName ++ "_entry"
      initialState =
        TranslationState
          { currentTemp = 0,
            labelCount = 0,
            blocks = [],
            envStack = []
          }
      (_, ts) = runState (transBlock entryLabel body) initialState
   in MirFunction
        { irFunName = funName,
          irFunArgs = funArgs,
          irFunEntryLabel = entryLabel,
          irFunBlocks = reverse ts.blocks
        }

transProgram :: TypedProgram -> MirProgram
transProgram Ast.Program {funcs} =
  let irFunctions = map transFun funcs
   in IrProgram {irFunctions = irFunctions}

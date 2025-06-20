{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Mir
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
import Control.Monad (foldM)
import Control.Monad.State (State, gets, modify, runState)
import Env qualified
import TypeCheck
  ( TypedBlock,
    TypedExp,
    TypedFun,
    TypedProgram,
    TypedStmt,
  )

type Temp = Int

type Label = String

data MirVar
  = Local Ast.Id
  | Arg Ast.Id
  deriving (Eq)

instance Show MirVar where
  show (Local id) = "local " ++ id
  show (Arg id) = "arg " ++ id

data MirOperand
  = ConstInt Int
  | Temp Temp
  | Var MirVar
  deriving (Eq)

instance Show MirOperand where
  show (ConstInt n) = "const " ++ show n
  show (Temp t) = "t" ++ show t
  show (Var (Local id)) = "local " ++ id
  show (Var (Arg id)) = "arg " ++ id

data MirInstr
  = Assign Temp MirOperand
  | BinOp Temp Ast.Operator Temp Temp
  | Load Temp MirVar
  | Store MirVar Temp
  | Call (Maybe Temp) Ast.Id Int
  | Param Temp
  | Return (Maybe Temp)
  | Jump Label
  | CondJump Temp Label Label
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
  { blockLabel :: Label,
    instrs :: [MirInstr]
  }
  deriving (Eq)

instance Show MirBasicBlock where
  show (MirBasicBlock label instructions) =
    label ++ ":\n" ++ unlines (map (("  " ++) . show) instructions)

data MirFunction = MirFunction
  { irFunName :: Ast.Id,
    irFunArgs :: [Ast.Id],
    irFunEntryLabel :: Label,
    irFunBlocks :: [MirBasicBlock]
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
      ++ unlines (reverse $ map show blocks)

newtype MirProgram = IrProgram
  { irFunctions :: [MirFunction]
  }
  deriving (Eq)

instance Show MirProgram where
  show (IrProgram functions) = unlines (map show functions)

data TranslationState = TranslationState
  { tmpCount :: Temp,
    labelCount :: Int,
    blocks :: [MirBasicBlock],
    envStack :: Env.EnvStack
  }

incTmp :: TranslationState -> TranslationState
incTmp ts@TranslationState {tmpCount} = ts {tmpCount = tmpCount + 1}

tmp :: TranslationState -> Temp
tmp TranslationState {tmpCount} = tmpCount

incLabel :: TranslationState -> TranslationState
incLabel ts@TranslationState {labelCount} = ts {labelCount = labelCount + 1}

label :: TranslationState -> Int
label TranslationState {labelCount} = labelCount

addInstsToBlock :: [MirInstr] -> TranslationState -> TranslationState
addInstsToBlock insts ts@TranslationState {blocks = curBlock : restBlocks} =
  ts {blocks = curBlock {instrs = curBlock.instrs ++ insts} : restBlocks}
addInstsToBlock _ TranslationState {blocks = []} =
  error "No current block to add instructions to"

pushBlock :: String -> TranslationState -> TranslationState
pushBlock label ts@TranslationState {blocks} =
  ts {blocks = MirBasicBlock {blockLabel = label, instrs = []} : blocks}

popBlocks :: TranslationState -> TranslationState
popBlocks ts = ts {blocks = []}

stackEnv :: Env.Env -> TranslationState -> TranslationState
stackEnv env ts@TranslationState {envStack} =
  ts {envStack = Env.pushEnv env envStack}

popEnv :: TranslationState -> TranslationState
popEnv ts@TranslationState {envStack} =
  ts {envStack = Env.popEnv envStack}

lookupId :: Ast.Id -> TranslationState -> Maybe Env.Symbol
lookupId id TranslationState {envStack} = Env.lookup id envStack

transExp :: TypedExp -> State TranslationState [MirInstr]
transExp Ast.Exp {annot, exp}
  | Ast.IdExp {id} <- exp = do
      symb <- gets (lookupId id)
      t <- gets tmp
      case symb of
        Just Env.Symbol {variant} ->
          case variant of
            Env.Local -> return [Load t (Local id)]
            Env.Argument -> return [Load t (Arg id)]
            _ -> error $ "Unexpected symbol variant for variable: " ++ show variant
        Nothing -> error $ "Undefined variable: " ++ id
  | Ast.NumberExp {num} <- exp = do
      t <- gets tmp
      return [Assign t (ConstInt num)]
  | Ast.BinExp {left, op, right} <- exp = do
      linst <- transExp left
      lt <- gets tmp
      modify incTmp
      rinst <- transExp right
      rt <- gets tmp
      modify incTmp
      tout <- gets tmp
      return (linst ++ rinst ++ [BinOp tout op lt rt])
  | Ast.Call {id, args} <- exp = do
      argInsts <-
        foldM
          ( \acc arg -> do
              insts <- transExp arg
              t <- gets tmp
              modify incTmp
              return (acc ++ insts ++ [Param t])
          )
          []
          args
      case annot of
        Ast.VoidTy -> do
          return (argInsts ++ [Call Nothing id (length args)])
        _ -> do
          t <- gets tmp
          return (argInsts ++ [Call (Just t) id (length args)])

transStmt :: TypedStmt -> State TranslationState ()
transStmt stmt
  | Ast.ExpStmt {exp} <- stmt = do
      insts <- transExp exp
      modify (addInstsToBlock insts)
  | Ast.LetStmt {vardef = Ast.VarDef {id}, exp} <- stmt = do
      insts <- transExp exp
      t <- gets tmp
      modify incTmp
      modify (addInstsToBlock (insts ++ [Store (Local id) t]))
  | Ast.AssignStmt {id, exp} <- stmt = do
      insts <- transExp exp
      symb <- gets (lookupId id)
      case symb of
        Just Env.Symbol {variant = Env.Local} -> do
          t <- gets tmp
          modify incTmp
          modify (addInstsToBlock (insts ++ [Store (Local id) t]))
        Just Env.Symbol {variant = Env.Argument} -> do
          t <- gets tmp
          modify incTmp
          modify (addInstsToBlock (insts ++ [Store (Arg id) t]))
        _ -> error $ "Undefined variable: " ++ id
  | Ast.ReturnStmt {retexp} <- stmt = do
      case retexp of
        Just exp -> do
          insts <- transExp exp
          t <- gets tmp
          modify incTmp
          modify (addInstsToBlock (insts ++ [Return (Just t)]))
        Nothing -> do
          modify (addInstsToBlock [Return Nothing])
  | Ast.IfStmt {cond, ifBody, elseBody} <- stmt = do
      l <- gets label
      modify incLabel
      let thenLabel = "if_then_" ++ show l
      condInstrs <- transExp cond

      case elseBody of
        Just elseBody -> do
          l <- gets label
          modify incLabel
          let elseLabel = "if_else_" ++ show l
          l <- gets label
          modify incLabel
          let endLabel = "if_end_" ++ show l

          t <- gets tmp
          modify (addInstsToBlock $ condInstrs ++ [CondJump t thenLabel elseLabel])
          _ <- transBlock thenLabel ifBody
          modify (addInstsToBlock [Jump endLabel])
          _ <- transBlock elseLabel elseBody
          modify (addInstsToBlock [Jump endLabel])
          modify (pushBlock endLabel)
        Nothing -> do
          l <- gets label
          modify incLabel
          let endLabel = "if_end_" ++ show l

          t <- gets tmp
          modify (addInstsToBlock $ condInstrs ++ [CondJump t thenLabel endLabel])
          _ <- transBlock thenLabel ifBody
          modify (addInstsToBlock [Jump endLabel])
          modify (pushBlock endLabel)
  | Ast.WhileStmt {cond, body} <- stmt = do
      l <- gets label
      modify incLabel
      let condLabel = "while_cond_" ++ show l
      l <- gets label
      modify incLabel
      let loopLabel = "while_loop_" ++ show l
      l <- gets label
      modify incLabel
      let endLabel = "while_end_" ++ show l
      modify incLabel
      condInstrs <- transExp cond
      modify (pushBlock condLabel)
      t <- gets tmp
      modify (addInstsToBlock $ condInstrs ++ [CondJump t loopLabel endLabel])
      _ <- transBlock loopLabel body
      modify (addInstsToBlock [Jump condLabel])
      modify (pushBlock endLabel)

transBlock :: String -> TypedBlock -> State TranslationState ()
transBlock label Ast.Block {annot = scope, stmts} = do
  modify (pushBlock label)
  modify (stackEnv scope)
  mapM_ transStmt stmts
  modify popEnv

transFun :: TypedFun -> State TranslationState MirFunction
transFun Ast.Fun {id, args, body} = do
  l <- gets label
  modify incLabel
  let entryLabel = "entry_" ++ show l
  _ <- transBlock entryLabel body
  blocks <- gets blocks
  modify popEnv
  modify popBlocks
  let args' = map (\Ast.VarDef {id} -> id) args
  return MirFunction {irFunName = id, irFunArgs = args', irFunEntryLabel = entryLabel, irFunBlocks = blocks}

transProgram :: TypedProgram -> MirProgram
transProgram Ast.Program {annot, funcs} =
  let initialState =
        TranslationState
          { tmpCount = 0,
            labelCount = 0,
            blocks = [],
            envStack = Env.pushEnv annot (Env.emptyEnvStack "global")
          }
      (mirFuncs, _) =
        runState
          ( foldM
              ( \acc fun -> do
                  mirFun <- transFun fun
                  return (acc ++ [mirFun])
              )
              []
              funcs
          )
          initialState
   in IrProgram {irFunctions = mirFuncs}

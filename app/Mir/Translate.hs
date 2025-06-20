{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Mir.Translate
  ( transProgram,
  )
where

import Ast qualified
import Ast.Semant qualified
import Ast.Types qualified
import Control.Monad (foldM)
import Control.Monad.State (State, gets, modify, runState)
import Env qualified
import Mir.Types qualified
import Prelude hiding (lookup)

data TranslationState = TranslationState
  { tmpCount :: Mir.Types.Temp,
    labelCount :: Int,
    blocks :: [Mir.Types.BasicBlock],
    envStack :: Env.EnvStack
  }

incTmp :: TranslationState -> TranslationState
incTmp ts@TranslationState {tmpCount} = ts {tmpCount = tmpCount + 1}

tmp :: TranslationState -> Mir.Types.Temp
tmp TranslationState {tmpCount} = tmpCount

incLabel :: TranslationState -> TranslationState
incLabel ts@TranslationState {labelCount} = ts {labelCount = labelCount + 1}

label :: TranslationState -> Int
label TranslationState {labelCount} = labelCount

addInstsToBlock :: [Mir.Types.Inst] -> TranslationState -> TranslationState
addInstsToBlock insts ts@TranslationState {blocks = curBlock : restBlocks} =
  ts {blocks = curBlock {Mir.Types.insts = curBlock.insts ++ insts} : restBlocks}
addInstsToBlock _ TranslationState {blocks = []} =
  error "No current block to add instructions to"

pushBlock :: String -> TranslationState -> TranslationState
pushBlock label ts@TranslationState {blocks} =
  ts {blocks = Mir.Types.BasicBlock {blockLabel = label, insts = []} : blocks}

popBlocks :: TranslationState -> TranslationState
popBlocks ts = ts {blocks = []}

stackEnv :: Env.Env -> TranslationState -> TranslationState
stackEnv env ts@TranslationState {envStack} =
  ts {envStack = Env.pushEnv env envStack}

popEnv :: TranslationState -> TranslationState
popEnv ts@TranslationState {envStack} =
  ts {envStack = Env.popEnv envStack}

lookup :: Ast.Id -> TranslationState -> Maybe Env.Symbol
lookup id TranslationState {envStack} = Env.lookup id envStack

transExp :: Ast.Semant.TypedExp -> State TranslationState [Mir.Types.Inst]
transExp Ast.Exp {annot, exp}
  | Ast.IdExp {id} <- exp = do
      symb <- gets (lookup id)
      t <- gets tmp
      case symb of
        Just Env.Symbol {variant} ->
          case variant of
            Env.Local -> return [Mir.Types.Load t (Mir.Types.Local id)]
            Env.Argument -> return [Mir.Types.Load t (Mir.Types.Arg id)]
            _ -> error $ "Unexpected symbol variant for variable: " ++ show variant
        Nothing -> error $ "Undefined variable: " ++ id
  | Ast.NumberExp {num} <- exp = do
      t <- gets tmp
      return [Mir.Types.Assign t (Mir.Types.ConstInt num)]
  | Ast.BinExp {left, op, right} <- exp = do
      linst <- transExp left
      lt <- gets tmp
      modify incTmp
      rinst <- transExp right
      rt <- gets tmp
      modify incTmp
      tout <- gets tmp
      return (linst ++ rinst ++ [Mir.Types.BinOp tout op lt rt])
  | Ast.Call {id, args} <- exp = do
      argInsts <-
        foldM
          ( \acc arg -> do
              insts <- transExp arg
              t <- gets tmp
              modify incTmp
              return (acc ++ insts ++ [Mir.Types.Param t])
          )
          []
          args
      case annot of
        Ast.VoidTy -> do
          return (argInsts ++ [Mir.Types.Call Nothing id (length args)])
        _ -> do
          t <- gets tmp
          return (argInsts ++ [Mir.Types.Call (Just t) id (length args)])

transStmt :: Ast.Semant.TypedStmt -> State TranslationState ()
transStmt stmt
  | Ast.ExpStmt {exp} <- stmt = do
      insts <- transExp exp
      modify (addInstsToBlock insts)
  | Ast.LetStmt {vardef = Ast.VarDef {id}, exp} <- stmt = do
      insts <- transExp exp
      t <- gets tmp
      modify incTmp
      modify (addInstsToBlock (insts ++ [Mir.Types.Store (Mir.Types.Local id) t]))
  | Ast.AssignStmt {id, exp} <- stmt = do
      insts <- transExp exp
      symb <- gets (lookup id)
      case symb of
        Just Env.Symbol {variant = Env.Local} -> do
          t <- gets tmp
          modify incTmp
          modify (addInstsToBlock (insts ++ [Mir.Types.Store (Mir.Types.Local id) t]))
        Just Env.Symbol {variant = Env.Argument} -> do
          t <- gets tmp
          modify incTmp
          modify (addInstsToBlock (insts ++ [Mir.Types.Store (Mir.Types.Arg id) t]))
        _ -> error $ "Undefined variable: " ++ id
  | Ast.ReturnStmt {retexp} <- stmt = do
      case retexp of
        Just exp -> do
          insts <- transExp exp
          t <- gets tmp
          modify incTmp
          modify (addInstsToBlock (insts ++ [Mir.Types.Return (Just t)]))
        Nothing -> do
          modify (addInstsToBlock [Mir.Types.Return Nothing])
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
          modify (addInstsToBlock $ condInstrs ++ [Mir.Types.CondJump t thenLabel elseLabel])
          _ <- transBlock thenLabel ifBody
          modify (addInstsToBlock [Mir.Types.Jump endLabel])
          _ <- transBlock elseLabel elseBody
          modify (addInstsToBlock [Mir.Types.Jump endLabel])
          modify (pushBlock endLabel)
        Nothing -> do
          l <- gets label
          modify incLabel
          let endLabel = "if_end_" ++ show l

          t <- gets tmp
          modify (addInstsToBlock $ condInstrs ++ [Mir.Types.CondJump t thenLabel endLabel])
          _ <- transBlock thenLabel ifBody
          modify (addInstsToBlock [Mir.Types.Jump endLabel])
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
      modify (addInstsToBlock $ condInstrs ++ [Mir.Types.CondJump t loopLabel endLabel])
      _ <- transBlock loopLabel body
      modify (addInstsToBlock [Mir.Types.Jump condLabel])
      modify (pushBlock endLabel)

transBlock :: String -> Ast.Semant.TypedBlock -> State TranslationState ()
transBlock label Ast.Block {annot = scope, stmts} = do
  modify (pushBlock label)
  modify (stackEnv scope)
  mapM_ transStmt stmts
  modify popEnv

transFun :: Ast.Semant.TypedFun -> State TranslationState Mir.Types.Fun
transFun Ast.Fun {id, args, body} = do
  l <- gets label
  modify incLabel
  let entryLabel = "entry_" ++ show l
  _ <- transBlock entryLabel body
  blocks <- gets blocks
  modify popBlocks
  let args' = map (\Ast.VarDef {id} -> id) args
  let Ast.Types.Block {annot} = body
  -- remove parameters from locals
  let locals = filter (`notElem` args') (Env.toIdList annot)
  return Mir.Types.Fun {id = id, args = args', locals, entryLabel = entryLabel, blocks = blocks}

transProgram :: Ast.Semant.TypedProgram -> Mir.Types.Program
transProgram Ast.Program {annot, funcs} = Mir.Types.Program {funs}
  where
    initialState =
      TranslationState
        { tmpCount = 0,
          labelCount = 0,
          blocks = [],
          envStack = Env.pushEnv annot (Env.emptyEnvStack "global")
        }
    (funs, _) =
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

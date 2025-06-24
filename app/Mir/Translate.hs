{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Mir.Translate
  ( transProgram,
  )
where

import Ast qualified
import Control.Monad (foldM)
import Control.Monad.State (State, gets, modify', runState)
import Env qualified
import Mir.Types qualified as Mir
import Prelude hiding (lookup)

data TranslationState = TranslationState
  { tmp :: Mir.Temp,
    label :: Int,
    blocks :: [Mir.BasicBlock],
    envs :: Env.EnvStack
  }

incTmp :: TranslationState -> TranslationState
incTmp ts = ts {tmp = ts.tmp + 1}

incLabel :: TranslationState -> TranslationState
incLabel ts = ts {label = ts.label + 1}

addInstsToBlock :: [Mir.Inst] -> TranslationState -> TranslationState
addInstsToBlock insts ts@TranslationState {blocks = curBlock : restBlocks} =
  ts {blocks = curBlock {Mir.insts = curBlock.insts ++ insts} : restBlocks}
addInstsToBlock _ TranslationState {blocks = []} =
  error "No current block to add instructions to"

pushBlock :: String -> TranslationState -> TranslationState
pushBlock label ts =
  ts {blocks = Mir.BasicBlock {label, insts = []} : ts.blocks}

popBlocks :: TranslationState -> TranslationState
popBlocks ts = ts {blocks = []}

stackEnv :: Env.Env -> TranslationState -> TranslationState
stackEnv env ts = ts {envs = Env.pushEnv env ts.envs}

popEnv :: TranslationState -> TranslationState
popEnv ts = ts {envs = Env.popEnv ts.envs}

lookup :: Ast.Id -> TranslationState -> Maybe Env.Symbol
lookup id ts = Env.lookup id ts.envs

transExp :: Ast.TypedExp -> State TranslationState ()
transExp Ast.Exp {annot, exp}
  | Ast.IdExp {id} <- exp = do
      symb <- gets (lookup id)
      t <- gets (.tmp)
      case symb of
        Just Env.Symbol {alloc} ->
          case alloc of
            Env.Local -> do
              modify' $ addInstsToBlock [Mir.Load {dst = t, srcVar = Mir.Local {id}}]
            Env.Argument ->
              modify' $ addInstsToBlock [Mir.Load {dst = t, srcVar = Mir.Arg {id}}]
            _ -> error $ "Unexpected symbol alloc for variable: " ++ show alloc
        Nothing -> error $ "Undefined variable: " ++ id
  | Ast.NumberExp {num} <- exp = do
      t <- gets (.tmp)
      modify' $ addInstsToBlock [Mir.Assign {dst = t, srcOp = Mir.ConstInt num}]
  | Ast.BinExp {left, op, right} <- exp = do
      transExp left
      lt <- gets (.tmp)
      modify' incTmp
      transExp right
      rt <- gets (.tmp)
      modify' incTmp
      tout <- gets (.tmp)
      modify' $ addInstsToBlock [Mir.BinOp {dst = tout, binop = op, left = lt, right = rt}]
  | Ast.UnaryExp {unop, exp} <- exp = do
      transExp exp
      t <- gets (.tmp)
      modify' incTmp
      tout <- gets (.tmp)
      modify' $ addInstsToBlock [Mir.UnaryOp {dst = tout, unop = unop, src = t}]
  | Ast.Call {id, args} <- exp = do
      mapM_
        ( \arg -> do
            transExp arg
            t <- gets (.tmp)
            modify' incTmp
            modify' $ addInstsToBlock [Mir.Param {param = t}]
        )
        args
      case annot of
        Ast.VoidTy -> do
          modify' $ addInstsToBlock [Mir.Call {ret = Nothing, funId = id, argCount = length args}]
        _ -> do
          t <- gets (.tmp)
          modify' $ addInstsToBlock [Mir.Call {ret = Just t, funId = id, argCount = length args}]

transStmt :: Ast.TypedStmt -> State TranslationState ()
transStmt stmt
  | Ast.ExpStmt {exp} <- stmt = do
      transExp exp
  | Ast.LetStmt {vardef = Ast.VarDef {id}, exp} <- stmt = do
      transExp exp
      t <- gets (.tmp)
      modify' incTmp
      modify' (addInstsToBlock [Mir.Store {dstVar = Mir.Local {id}, src = t}])
  | Ast.AssignStmt {id, exp} <- stmt = do
      transExp exp
      symb <- gets (lookup id)
      case symb of
        Just Env.Symbol {alloc = Env.Local} -> do
          t <- gets (.tmp)
          modify' incTmp
          modify' (addInstsToBlock [Mir.Store {dstVar = Mir.Local {id}, src = t}])
        Just Env.Symbol {alloc = Env.Argument} -> do
          t <- gets (.tmp)
          modify' incTmp
          modify' (addInstsToBlock [Mir.Store {dstVar = Mir.Arg {id}, src = t}])
        _ -> error $ "Undefined variable: " ++ id
  | Ast.ReturnStmt {retexp} <- stmt = do
      case retexp of
        Just exp -> do
          transExp exp
          t <- gets (.tmp)
          modify' incTmp
          modify' (addInstsToBlock [Mir.Return {retVal = Just t}])
        Nothing -> do
          modify' (addInstsToBlock [Mir.Return {retVal = Nothing}])
  | Ast.IfStmt {cond, ifBody, elseBody} <- stmt = do
      l <- gets (.label)
      modify' incLabel
      let thenLabel = "if_then_" ++ show l
      transExp cond

      case elseBody of
        Just elseBody -> do
          l <- gets (.label)
          modify' incLabel
          let elseLabel = "if_else_" ++ show l
          l <- gets (.label)
          modify' incLabel
          let endLabel = "if_end_" ++ show l

          t <- gets (.tmp)
          modify' (addInstsToBlock [Mir.CondJump {cond = t, trueLabel = thenLabel, falseLabel = elseLabel}])
          _ <- transBlock thenLabel ifBody
          modify' (addInstsToBlock [Mir.Jump {target = endLabel}])
          _ <- transBlock elseLabel elseBody
          modify' (addInstsToBlock [Mir.Jump {target = endLabel}])
          modify' (pushBlock endLabel)
        Nothing -> do
          l <- gets (.label)
          modify' incLabel
          let endLabel = "if_end_" ++ show l

          t <- gets (.tmp)
          modify' (addInstsToBlock [Mir.CondJump {cond = t, trueLabel = thenLabel, falseLabel = endLabel}])
          _ <- transBlock thenLabel ifBody
          modify' (addInstsToBlock [Mir.Jump {target = endLabel}])
          modify' (pushBlock endLabel)
  | Ast.WhileStmt {cond, body} <- stmt = do
      l <- gets (.label)
      modify' incLabel
      let condLabel = "while_cond_" ++ show l
      l <- gets (.label)
      modify' incLabel
      let loopLabel = "while_loop_" ++ show l
      l <- gets (.label)
      modify' incLabel
      let endLabel = "while_end_" ++ show l
      modify' incLabel

      modify' (pushBlock condLabel)
      transExp cond
      t <- gets (.tmp)
      modify' (addInstsToBlock [Mir.CondJump {cond = t, trueLabel = loopLabel, falseLabel = endLabel}])

      _ <- transBlock loopLabel body
      modify' (addInstsToBlock [Mir.Jump {target = condLabel}])

      modify' (pushBlock endLabel)

transBlock :: String -> Ast.TypedBlock -> State TranslationState ()
transBlock label Ast.Block {annot = scope, stmts} = do
  modify' (pushBlock label)
  modify' (stackEnv scope)
  mapM_ transStmt stmts
  modify' popEnv

transFun :: Ast.TypedFun -> State TranslationState Mir.Fun
transFun Ast.Fun {id, args, body} = do
  let entryLabel = id ++ "_entry"
  _ <- transBlock entryLabel body
  blocks <- gets (.blocks)
  modify' popBlocks
  let args' = (.id) <$> args
  let Ast.Block {annot} = body
  -- remove parameters from locals
  let locals = filter (`notElem` args') (Env.toIdList annot)
  return Mir.Fun {id = id, args = args', locals, entryLabel = entryLabel, blocks = blocks}

transExternFun :: Ast.ExternFun -> Mir.ExternFun
transExternFun Ast.ExternFun {id} = Mir.ExternFun {externId = id}

transProgram :: Ast.TypedProgram -> Mir.Program
transProgram Ast.Program {annot, funcs, externFuns, mainFun} = do
  let externFuns' = transExternFun <$> externFuns
  let initialState =
        TranslationState
          { tmp = 0,
            label = 0,
            blocks = [],
            envs = Env.pushEnv annot (Env.emptyEnvStack "global")
          }

  let (funs, st) =
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

  let mainFun' = case mainFun of
        Just fun -> Just (runState (transFun fun) st)
        Nothing -> Nothing

  Mir.Program {funs, externFuns = externFuns', mainFun = fst <$> mainFun'}

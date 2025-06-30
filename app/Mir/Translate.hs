{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Mir.Translate
  ( transProgram,
  )
where

import Ast qualified
import Control.Monad (foldM, foldM_)
import Control.Monad.State (State, gets, modify', runState)
import Env qualified
import Mir.Types qualified as Mir
import TypeSystem (Id, Ty (..), sizeOf)
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

lookup :: Id -> TranslationState -> Maybe Env.Symbol
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
      modify' $ addInstsToBlock [Mir.Mov {dst = t, srcOp = Mir.ConstInt num}]
  | Ast.CharExp {char} <- exp = do
      t <- gets (.tmp)
      modify' $ addInstsToBlock [Mir.Mov {dst = t, srcOp = Mir.ConstChar char}]
  | Ast.BinExp {left, op, right} <- exp = do
      case (left.exp, right.exp) of
        (Ast.NumberExp {num = l}, Ast.NumberExp {num = r}) -> do
          t <- gets (.tmp)
          modify' $ addInstsToBlock [Mir.BinOp {dst = t, binop = op, left = Mir.ConstInt l, right = Mir.ConstInt r}]
        (Ast.NumberExp {num = l}, _) -> do
          transExp right
          t <- gets (.tmp)
          modify' incTmp
          modify' $ addInstsToBlock [Mir.BinOp {dst = t, binop = op, left = Mir.ConstInt l, right = Mir.Temp t}]
        (_, Ast.NumberExp {num = r}) -> do
          transExp left
          t <- gets (.tmp)
          modify' incTmp
          modify' $ addInstsToBlock [Mir.BinOp {dst = t, binop = op, left = Mir.Temp t, right = Mir.ConstInt r}]
        (Ast.CharExp {char = l}, Ast.CharExp {char = r}) -> do
          t <- gets (.tmp)
          modify' $ addInstsToBlock [Mir.BinOp {dst = t, binop = op, left = Mir.ConstChar l, right = Mir.ConstChar r}]
        (Ast.CharExp {char = l}, _) -> do
          transExp right
          t <- gets (.tmp)
          modify' incTmp
          modify' $ addInstsToBlock [Mir.BinOp {dst = t, binop = op, left = Mir.ConstChar l, right = Mir.Temp t}]
        (_, Ast.CharExp {char = r}) -> do
          transExp left
          t <- gets (.tmp)
          modify' incTmp
          modify' $ addInstsToBlock [Mir.BinOp {dst = t, binop = op, left = Mir.Temp t, right = Mir.ConstChar r}]
        _ -> do
          transExp left
          lt <- gets (.tmp)
          modify' incTmp
          transExp right
          rt <- gets (.tmp)
          modify' $ addInstsToBlock [Mir.BinOp {dst = rt, binop = op, left = Mir.Temp lt, right = Mir.Temp rt}]
  | Ast.UnaryExp {unop, exp = Ast.Exp {exp = Ast.NumberExp {num}}} <- exp = do
      t <- gets (.tmp)
      modify' $ addInstsToBlock [Mir.UnaryOp {dst = t, unop = unop, unsrc = Mir.ConstInt num}]
  | Ast.UnaryExp {unop, exp} <- exp = do
      transExp exp
      t <- gets (.tmp)
      modify' $ addInstsToBlock [Mir.UnaryOp {dst = t, unop = unop, unsrc = Mir.Temp t}]
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
        VoidTy -> do
          modify' $ addInstsToBlock [Mir.Call {ret = Nothing, funId = id, argCount = length args}]
        _ -> do
          t <- gets (.tmp)
          modify' $ addInstsToBlock [Mir.Call {ret = Just t, funId = id, argCount = length args}]
  | Ast.ArrAccess {id, index = Ast.Exp {exp = Ast.NumberExp {num}}} <- exp = do
      -- Accessing an array with a constant index
      let idx = Mir.ConstInt num
      srcVar <- arrayAccess id idx
      t <- gets (.tmp)
      modify' incTmp
      modify' $ addInstsToBlock [Mir.Load {dst = t, srcVar}]
  | Ast.ArrAccess {id, index} <- exp = do
      transExp index
      tIndex <- gets (.tmp)
      modify' incTmp
      srcVar <- arrayAccess id (Mir.Temp tIndex)
      modify' $ addInstsToBlock [Mir.Load {dst = tIndex, srcVar}]

arrayAccess :: Id -> Mir.Operand -> State TranslationState Mir.Var
arrayAccess id (Mir.Temp idxtemp) = do
  symb <- gets (lookup id)
  case symb of
    Just Env.Symbol {alloc = Env.Local, ty = ArrTy {elemTy}} -> do
      return $ Mir.LocalWithOffset {id, offset = Mir.Temp idxtemp, mult = sizeOf elemTy}
    Just Env.Symbol {alloc = Env.Argument} -> do
      error "Array access on argument is not supported"
    _ -> error $ "Undefined array: " ++ id
arrayAccess id (Mir.ConstInt idx) = do
  symb <- gets (lookup id)
  case symb of
    Just Env.Symbol {alloc = Env.Local, ty = ArrTy {elemTy}} -> do
      return $ Mir.LocalWithOffset {id, offset = Mir.ConstInt idx, mult = sizeOf elemTy}
    Just Env.Symbol {alloc = Env.Argument} -> do
      error "Array access on argument is not supported"
    _ -> error $ "Undefined array: " ++ id

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
  | Ast.LetArrStmt {vardef = Ast.VarDef {id}, elems} <- stmt = do
      -- Store all the elements in the initializer in the array
      -- SInce the array is allocated on the stack, we can use the temporary
      -- registers to store the elements
      foldM_
        ( \idx elem -> do
            -- access the array and store the element
            dstVar <- arrayAccess id (Mir.ConstInt idx)
            transExp elem
            tElem <- gets (.tmp)
            modify' incTmp
            modify' (addInstsToBlock [Mir.Store {dstVar, src = tElem}])

            return (idx + 1)
        )
        0
        elems
  | Ast.AssignArrStmt {id, index = Ast.Exp {exp = Ast.NumberExp {num}}, exp} <- stmt = do
      -- Assigning to an array with a constant index
      let idx = Mir.ConstInt num
      dstVar <- arrayAccess id idx
      transExp exp
      tExp <- gets (.tmp)
      modify' incTmp
      modify' (addInstsToBlock [Mir.Store {dstVar, src = tExp}])
  | Ast.AssignArrStmt {id, index, exp} <- stmt = do
      transExp index
      tIndex <- gets (.tmp)
      modify' incTmp

      dstVar <- arrayAccess id (Mir.Temp tIndex)
      transExp exp
      tExp <- gets (.tmp)
      modify' incTmp
      modify' (addInstsToBlock [Mir.Store {dstVar, src = tExp}])
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

  -- let args' = args
  let Ast.Block {annot} = body

  -- for each argument find its symbol in the environment, order matters
  modify' (stackEnv annot)

  args <-
    mapM
      ( \arg -> do
          symb <- gets (lookup arg.id)
          case symb of
            Just s@Env.Symbol {alloc} ->
              case alloc of
                Env.Argument -> return s
                _ -> error $ "Unexpected symbol alloc for argument: " ++ show alloc
            Nothing -> error $ "Undefined argument: " ++ arg.id
      )
      args

  modify' popEnv

  -- for each local variable find its symbol in the environment, order does not matter
  let locals =
        filter
          ( \local -> case local.alloc of
              Env.Local -> True
              _ -> False
          )
          (Env.toList annot)

  _ <- transBlock entryLabel body
  blocks <- gets (.blocks)
  modify' popBlocks

  return Mir.Fun {id, args, locals, entryLabel, blocks}

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

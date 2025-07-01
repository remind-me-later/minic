{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Mir.Translate (transProgram) where

import Ast qualified
import Control.Monad (foldM, foldM_, unless)
import Control.Monad.State (State, gets, modify', runState)
import Data.Set qualified as Set
import Env qualified
import Mir.Types
import TypeSystem (Id, Ty (..), sizeOf)
import Prelude hiding (lookup)

data TranslationState = TranslationState
  { tmp :: Temp,
    blockId :: Int,
    curBlockId :: BlockId,
    currentInsts :: [Inst],
    blocks :: [BasicBlock],
    envs :: Env.EnvStack
  }

incTmp :: TranslationState -> TranslationState
incTmp ts = ts {tmp = ts.tmp + 1}

incBlockId :: TranslationState -> TranslationState
incBlockId ts = ts {blockId = ts.blockId + 1}

addInstsToBlock :: [Inst] -> TranslationState -> TranslationState
addInstsToBlock insts ts@TranslationState {currentInsts = curInsts} =
  ts {currentInsts = curInsts ++ insts}

setCurBlockId :: BlockId -> TranslationState -> TranslationState
setCurBlockId blockId ts = ts {curBlockId = blockId}

cfgFromBlocks :: [BasicBlock] -> CFG
cfgFromBlocks blocks =
  let entryBlockId = case blocks of
        [] -> error "No blocks to create CFG"
        (BasicBlock {blockId} : _) -> blockId
      exitBlocks =
        foldr
          ( \(BasicBlock {terminator}) acc -> case terminator of
              (Return {}) -> acc ++ [entryBlockId]
              _ -> acc
          )
          mempty
          blocks
   in CFG {entryBlockId, exitBlocks = Set.fromList exitBlocks, blocks}

terminateBlock :: Terminator -> TranslationState -> TranslationState
terminateBlock terminator ts =
  ts
    { blocks = BasicBlock {blockId = ts.curBlockId, insts = ts.currentInsts, terminator} : ts.blocks,
      currentInsts = [],
      curBlockId = "terminated_"
    }

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
              modify' $ addInstsToBlock [Load {dst = Temp t, srcVar = Local {id}}]
            Env.Argument ->
              modify' $ addInstsToBlock [Load {dst = Temp t, srcVar = Arg {id}}]
            _ -> error $ "Unexpected symbol alloc for variable: " ++ show alloc
        Nothing -> error $ "Undefined variable: " ++ id
  | Ast.NumberExp {num} <- exp = do
      t <- gets (.tmp)
      modify' $ addInstsToBlock [Assign {dst = Temp t, src = ConstInt num}]
  | Ast.CharExp {char} <- exp = do
      t <- gets (.tmp)
      modify' $ addInstsToBlock [Assign {dst = Temp t, src = ConstChar char}]
  | Ast.BinExp {left, op, right} <- exp = do
      case (left.exp, right.exp) of
        (Ast.NumberExp {num = l}, Ast.NumberExp {num = r}) -> do
          t <- gets (.tmp)
          modify' $ addInstsToBlock [BinOp {dst = Temp t, binop = op, left = ConstInt l, right = ConstInt r}]
        (Ast.NumberExp {num = l}, _) -> do
          transExp right
          t <- gets (.tmp)
          modify' $ addInstsToBlock [BinOp {dst = Temp t, binop = op, left = ConstInt l, right = Temp t}]
        (_, Ast.NumberExp {num = r}) -> do
          transExp left
          t <- gets (.tmp)
          modify' $ addInstsToBlock [BinOp {dst = Temp t, binop = op, left = Temp t, right = ConstInt r}]
        (Ast.CharExp {char = l}, Ast.CharExp {char = r}) -> do
          t <- gets (.tmp)
          modify' $ addInstsToBlock [BinOp {dst = Temp t, binop = op, left = ConstChar l, right = ConstChar r}]
        (Ast.CharExp {char = l}, _) -> do
          transExp right
          t <- gets (.tmp)
          modify' $ addInstsToBlock [BinOp {dst = Temp t, binop = op, left = ConstChar l, right = Temp t}]
        (_, Ast.CharExp {char = r}) -> do
          transExp left
          t <- gets (.tmp)
          modify' $ addInstsToBlock [BinOp {dst = Temp t, binop = op, left = Temp t, right = ConstChar r}]
        _ -> do
          transExp left
          lt <- gets (.tmp)
          modify' incTmp
          transExp right
          rt <- gets (.tmp)
          modify' $ addInstsToBlock [BinOp {dst = Temp rt, binop = op, left = Temp lt, right = Temp rt}]
  | Ast.UnaryExp {unop, exp = Ast.Exp {exp = Ast.NumberExp {num}}} <- exp = do
      t <- gets (.tmp)
      modify' $ addInstsToBlock [UnaryOp {dst = Temp t, unop = unop, src = ConstInt num}]
  | Ast.UnaryExp {unop, exp} <- exp = do
      transExp exp
      t <- gets (.tmp)
      modify' $ addInstsToBlock [UnaryOp {dst = Temp t, unop = unop, src = Temp t}]
  | Ast.Call {id, args} <- exp = do
      mapM_
        ( \arg -> do
            transExp arg
            t <- gets (.tmp)
            modify' incTmp
            modify' $ addInstsToBlock [Param {param = Temp t}]
        )
        args
      case annot of
        VoidTy -> do
          modify' $ addInstsToBlock [Call {ret = Nothing, funId = id, argCount = length args}]
        _ -> do
          t <- gets (.tmp)
          modify' $ addInstsToBlock [Call {ret = Just $ Temp t, funId = id, argCount = length args}]
  | Ast.ArrAccess {id, index = Ast.Exp {exp = Ast.NumberExp {num}}} <- exp = do
      -- Accessing an array with a constant index
      let idx = ConstInt num
      srcVar <- arrayAccess id idx
      t <- gets (.tmp)
      modify' $ addInstsToBlock [Load {dst = Temp t, srcVar}]
  | Ast.ArrAccess {id, index} <- exp = do
      transExp index
      tIndex <- gets (.tmp)
      srcVar <- arrayAccess id (Temp tIndex)
      modify' $ addInstsToBlock [Load {dst = Temp tIndex, srcVar}]

arrayAccess :: Id -> Operand -> State TranslationState Var
arrayAccess id (Temp idxtemp) = do
  symb <- gets (lookup id)
  case symb of
    Just Env.Symbol {alloc = Env.Local, ty = ArrTy {elemTy}} -> do
      return $ LocalWithOffset {id, offset = Temp idxtemp, mult = sizeOf elemTy}
    Just Env.Symbol {alloc = Env.Argument} -> do
      error "Array access on argument is not supported"
    _ -> error $ "Undefined array: " ++ id
arrayAccess id (ConstInt idx) = do
  symb <- gets (lookup id)
  case symb of
    Just Env.Symbol {alloc = Env.Local, ty = ArrTy {elemTy}} -> do
      return $ LocalWithOffset {id, offset = ConstInt idx, mult = sizeOf elemTy}
    Just Env.Symbol {alloc = Env.Argument} -> do
      error "Array access on argument is not supported"
    _ -> error $ "Undefined array: " ++ id
-- unsupported
arrayAccess id _ = error $ "Array access with unsupported index type for array: " ++ id

transStmt :: Ast.TypedStmt -> State TranslationState ()
transStmt stmt
  | Ast.ExpStmt {exp} <- stmt = do
      transExp exp
  | Ast.LetStmt {vardef = Ast.VarDef {id}, exp} <- stmt = do
      transExp exp
      t <- gets (.tmp)
      modify' (addInstsToBlock [Store {dstVar = Local {id}, src = Temp t}])
  | Ast.AssignStmt {id, exp} <- stmt = do
      transExp exp
      symb <- gets (lookup id)
      case symb of
        Just Env.Symbol {alloc = Env.Local} -> do
          t <- gets (.tmp)
          modify' (addInstsToBlock [Store {dstVar = Local {id}, src = Temp t}])
        Just Env.Symbol {alloc = Env.Argument} -> do
          t <- gets (.tmp)
          modify' (addInstsToBlock [Store {dstVar = Arg {id}, src = Temp t}])
        _ -> error $ "Undefined variable: " ++ id
  | Ast.LetArrStmt {vardef = Ast.VarDef {id}, elems} <- stmt = do
      -- Store all the elements in the initializer in the array
      -- SInce the array is allocated on the stack, we can use the temporary
      -- registers to store the elements
      foldM_
        ( \idx elem -> do
            -- access the array and store the element
            dstVar <- arrayAccess id (ConstInt idx)
            transExp elem
            tElem <- gets (.tmp)
            modify' incTmp
            modify' (addInstsToBlock [Store {dstVar, src = Temp tElem}])

            return (idx + 1)
        )
        0
        elems
  | Ast.AssignArrStmt {id, index = Ast.Exp {exp = Ast.NumberExp {num}}, exp} <- stmt = do
      -- Assigning to an array with a constant index
      let idx = ConstInt num
      dstVar <- arrayAccess id idx
      transExp exp
      tExp <- gets (.tmp)
      modify' (addInstsToBlock [Store {dstVar, src = Temp tExp}])
  | Ast.AssignArrStmt {id, index, exp} <- stmt = do
      transExp index
      tIndex <- gets (.tmp)
      modify' incTmp

      dstVar <- arrayAccess id (Temp tIndex)
      transExp exp
      tExp <- gets (.tmp)
      modify' (addInstsToBlock [Store {dstVar, src = Temp tExp}])
  | Ast.ReturnStmt {retexp} <- stmt = do
      case retexp of
        Just exp -> do
          transExp exp
          t <- gets (.tmp)
          -- modify' (addInstsToBlock [Return {retVal = Just t}])
          modify' $ terminateBlock Return {retVal = Just t}
        Nothing -> do
          modify' $ terminateBlock Return {retVal = Nothing}
  | Ast.IfStmt {cond, ifBody, elseBody} <- stmt = do
      l <- gets (.blockId)
      modify' incBlockId
      let thenBlockId = "IL" ++ show l
      transExp cond

      case elseBody of
        Just elseBody -> do
          l <- gets (.blockId)
          modify' incBlockId
          let elseBlockId = "IL" ++ show l
          l <- gets (.blockId)
          modify' incBlockId
          let endBlockId = "IL" ++ show l

          t <- gets (.tmp)
          modify' $ terminateBlock CondJump {cond = t, trueBlockId = thenBlockId, falseBlockId = elseBlockId}
          _ <- transBlock thenBlockId ifBody
          modify' $ terminateBlock Jump {target = endBlockId}
          _ <- transBlock elseBlockId elseBody
          modify' $ terminateBlock Jump {target = endBlockId}
          modify' (setCurBlockId endBlockId)
        Nothing -> do
          l <- gets (.blockId)
          modify' incBlockId
          let endBlockId = "if_end_" ++ show l

          t <- gets (.tmp)
          modify' $ terminateBlock CondJump {cond = t, trueBlockId = thenBlockId, falseBlockId = endBlockId}
          _ <- transBlock thenBlockId ifBody
          modify' $ terminateBlock Jump {target = endBlockId}
          modify' (setCurBlockId endBlockId)
  | Ast.WhileStmt {cond, body} <- stmt = do
      l <- gets (.blockId)
      modify' incBlockId
      let condBlockId = "WL" ++ show l
      l <- gets (.blockId)
      modify' incBlockId
      let loopBlockId = "WL" ++ show l
      l <- gets (.blockId)
      modify' incBlockId
      let endBlockId = "WL" ++ show l
      modify' incBlockId

      modify' $ terminateBlock Jump {target = condBlockId}

      modify' (setCurBlockId condBlockId)
      transExp cond
      t <- gets (.tmp)
      modify' $ terminateBlock CondJump {cond = t, trueBlockId = loopBlockId, falseBlockId = endBlockId}

      _ <- transBlock loopBlockId body
      modify' $ terminateBlock Jump {target = condBlockId}

      modify' (setCurBlockId endBlockId)

transBlock :: String -> Ast.TypedBlock -> State TranslationState ()
transBlock blockId Ast.Block {annot = scope, stmts} = do
  modify' (setCurBlockId blockId)
  modify' (stackEnv scope)
  mapM_ transStmt stmts
  modify' popEnv

transFun :: Ast.TypedFun -> State TranslationState Fun
transFun Ast.Fun {id, args, body} = do
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

  transBlock id body

  insts <- gets (.currentInsts)
  unless (null insts) $
    modify' (terminateBlock Return {retVal = Nothing})

  blockId <- gets (.curBlockId)
  unless (blockId == "terminated_") $
    modify' (terminateBlock Return {retVal = Nothing})

  cfg <- gets (cfgFromBlocks . reverse . (.blocks))

  modify' popBlocks

  return Fun {id, args, locals, cfg}

transExternFun :: Ast.ExternFun -> ExternFun
transExternFun Ast.ExternFun {id} = ExternFun {externId = id}

transProgram :: Ast.TypedProgram -> Program
transProgram Ast.Program {annot, funcs, externFuns, mainFun} = do
  let externFuns' = transExternFun <$> externFuns
  let initialState =
        TranslationState
          { tmp = 0,
            blockId = 0,
            blocks = [],
            envs = Env.pushEnv annot (Env.emptyEnvStack "global"),
            curBlockId = "global_entry",
            currentInsts = []
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

  Program {funs, externFuns = externFuns', mainFun = fst <$> mainFun'}

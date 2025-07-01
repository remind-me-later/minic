{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Mir.Translate (transProgram) where

import Ast qualified
import Control.Monad (foldM, foldM_, unless)
import Control.Monad.State (State, gets, modify', runState)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Env qualified
import Mir.Types
import TypeSystem (BinOp (..), Id, Ty (..), sizeOf)
import Prelude hiding (lookup)

data TranslationState = TranslationState
  { tmp :: Temp,
    blockId :: Int,
    curBlockId :: BlockId,
    currentInsts :: [Inst],
    blocks :: [BasicBlock],
    envs :: Env.EnvStack,
    stackOffsets :: Map.Map Id Int,
    currentLocalOffset :: Int, -- For locals (negative from RBP)
    currentArgOffset :: Int -- For arguments (positive from RBP)
  }

-- Allocate stack slot for arguments (positive offsets from RBP)
allocateArgSlot :: Id -> Ty -> State TranslationState Int
allocateArgSlot varId ty = do
  currentOffset <- gets (.currentArgOffset)
  let size = sizeOf ty
  let newOffset = currentOffset + size
  modify' $ \s ->
    s
      { stackOffsets = Map.insert varId currentOffset s.stackOffsets,
        currentArgOffset = newOffset
      }
  return currentOffset

-- Allocate stack slot for locals (negative offsets from RBP)
allocateLocalSlot :: Id -> Ty -> State TranslationState Int
allocateLocalSlot varId ty = do
  currentOffset <- gets (.currentLocalOffset)
  let size = sizeOf ty
  let newOffset = currentOffset - size -- Grow downward
  modify' $ \s ->
    s
      { stackOffsets = Map.insert varId newOffset s.stackOffsets,
        currentLocalOffset = newOffset
      }
  return newOffset

-- Get the stack offset for a variable
getStackOffset :: Id -> State TranslationState (Maybe Int)
getStackOffset varId = gets (Map.lookup varId . (.stackOffsets))

-- Convert identifier to StackOperand
idToStackOperand :: Id -> State TranslationState Operand
idToStackOperand varId = do
  maybeOffset <- getStackOffset varId
  case maybeOffset of
    Just offset -> return $ StackOperand offset
    Nothing -> error $ "Variable " ++ varId ++ " not allocated on stack"

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
      t <- gets (.tmp)
      stackOp <- idToStackOperand id
      modify' $ addInstsToBlock [Assign {dst = Temp t, src = stackOp}]
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
      stackOp <- arrayAccess id idx
      t <- gets (.tmp)
      modify' $ addInstsToBlock [Assign {dst = Temp t, src = stackOp}]
  | Ast.ArrAccess {id, index} <- exp = do
      transExp index
      t <- gets (.tmp)
      stackOp <- arrayAccess id (Temp t)
      modify' $ addInstsToBlock [Assign {dst = Temp t, src = stackOp}]

arrayAccess :: Id -> Operand -> State TranslationState Operand
arrayAccess id (ConstInt idx) = do
  symb <- gets (lookup id)
  baseOffset <- getStackOffset id
  case (symb, baseOffset) of
    (Just Env.Symbol {ty = ArrTy {elemTy}}, Just offset) -> do
      let elemSize = sizeOf elemTy
      return $ StackOperand (offset + idx * elemSize)
    _ -> error $ "Array access error for: " ++ id
arrayAccess id (Temp idxTemp) = do
  symb <- gets (lookup id)
  baseOffset <- getStackOffset id
  case (symb, baseOffset) of
    (Just Env.Symbol {ty = ArrTy {elemTy}}, Just offset) -> do
      let elemSize = sizeOf elemTy
      -- Create a computed address temp
      t <- gets (.tmp)
      modify' incTmp
      modify' $
        addInstsToBlock
          [ BinOp {dst = Temp t, binop = TypeSystem.Mul, left = Temp idxTemp, right = ConstInt elemSize},
            BinOp {dst = Temp t, binop = TypeSystem.Add, left = Temp t, right = ConstInt offset}
          ]
      return $ Temp t -- This temp holds the computed stack offset
    _ -> error $ "Array access error for: " ++ id
arrayAccess id _ = do
  -- This case should not happen, as we expect either ConstInt or Temp for index
  error $ "Invalid array access for: " ++ id

transStmt :: Ast.TypedStmt -> State TranslationState ()
transStmt stmt
  | Ast.ExpStmt {exp} <- stmt = do
      transExp exp
  | Ast.LetStmt {vardef = Ast.VarDef {id, ty}, exp} <- stmt = do
      _ <- allocateLocalSlot id ty
      transExp exp
      t <- gets (.tmp)
      stackOp <- idToStackOperand id
      modify' (addInstsToBlock [Assign {dst = stackOp, src = Temp t}])
  | Ast.AssignStmt {id, exp} <- stmt = do
      transExp exp
      t <- gets (.tmp)
      stackOp <- idToStackOperand id
      modify' (addInstsToBlock [Assign {dst = stackOp, src = Temp t}])
  | Ast.LetArrStmt {vardef = Ast.VarDef {id}, elems} <- stmt = do
      -- Store all the elements in the initializer in the array
      -- SInce the array is allocated on the stack, we can use the temporary
      -- registers to store the elements
      foldM_
        ( \idx elem -> do
            -- access the array and store the element
            dstOp <- arrayAccess id (ConstInt idx)
            transExp elem
            tElem <- gets (.tmp)
            modify' (addInstsToBlock [Assign {dst = dstOp, src = Temp tElem}])

            return (idx + 1)
        )
        0
        elems
  | Ast.AssignArrStmt {id, index = Ast.Exp {exp = Ast.NumberExp {num}}, exp} <- stmt = do
      -- Assigning to an array with a constant index
      let idx = ConstInt num
      dstOp <- arrayAccess id idx
      transExp exp
      tExp <- gets (.tmp)
      modify' (addInstsToBlock [Assign {dst = dstOp, src = Temp tExp}])
  | Ast.AssignArrStmt {id, index, exp} <- stmt = do
      transExp index
      tIndex <- gets (.tmp)
      modify' incTmp

      dstOp <- arrayAccess id (Temp tIndex)
      transExp exp
      tExp <- gets (.tmp)
      modify' (addInstsToBlock [Assign {dst = dstOp, src = Temp tExp}])
  | Ast.ReturnStmt {retexp} <- stmt = do
      case retexp of
        Just exp -> do
          transExp exp
          t <- gets (.tmp)
          -- modify' (addInstsToBlock [Return {retVal = Just t}])
          modify' $ terminateBlock Return {retVal = Just $ Temp t}
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
          modify' $ terminateBlock CondJump {cond = Temp t, trueBlockId = thenBlockId, falseBlockId = elseBlockId}
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
          modify' $ terminateBlock CondJump {cond = Temp t, trueBlockId = thenBlockId, falseBlockId = endBlockId}
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
      modify' $ terminateBlock CondJump {cond = Temp t, trueBlockId = loopBlockId, falseBlockId = endBlockId}

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
  let Ast.Block {annot} = body

  -- Reset stack allocation for this function
  -- Args start at RBP + 16 (skip return addr + saved RBP)
  -- Locals start at RBP - 8 (first local below RBP)
  modify' $ \s ->
    s
      { stackOffsets = Map.empty,
        currentArgOffset = 16, -- Start after saved RBP + return address
        currentLocalOffset = 0 -- Start at RBP, grow downward
      }

  -- Push the function's environment (which should contain the arguments)
  modify' (stackEnv annot)

  -- Now allocate stack space for arguments (they should already be in the env)
  args' <-
    mapM
      ( \arg -> do
          _ <- allocateArgSlot arg.id arg.ty -- Add to stackOffsets
          symb <- gets (lookup arg.id) -- Look up in environment
          case symb of
            Just s@Env.Symbol {alloc = Env.Argument} -> return s
            _ -> error $ "Argument allocation error: " ++ arg.id
      )
      args

  -- Allocate stack space for locals
  let locals =
        filter
          ( \local -> case local.alloc of
              Env.Local -> True
              _ -> False
          )
          (Env.toList annot)

  mapM_ (\local -> allocateLocalSlot local.id local.ty) locals

  -- Rest of function translation...
  transBlock id body

  insts <- gets (.currentInsts)
  unless (null insts) $
    modify' (terminateBlock Return {retVal = Nothing})

  blockId <- gets (.curBlockId)
  unless (blockId == "terminated_") $
    modify' (terminateBlock Return {retVal = Nothing})

  cfg <- gets (cfgFromBlocks . reverse . (.blocks))

  modify' popBlocks

  return Fun {id, args = args', locals, cfg}

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
            currentInsts = [],
            stackOffsets = mempty,
            currentLocalOffset = 0,
            currentArgOffset = 0
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

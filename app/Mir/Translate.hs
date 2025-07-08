{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Mir.Translate (transProgram) where

import Ast qualified
import Ast.Types
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
    varToTemp :: Map.Map Id Temp,
    stackOffsets :: Map.Map Id Int,
    currentLocalOffset :: Int, -- For locals (negative from RBP)
    currentArgOffset :: Int -- For arguments (positive from RBP)
  }

allocateLocalToTmp :: Id -> State TranslationState Temp
allocateLocalToTmp varId = do
  currentTemp <- gets tmp
  modify' $ \s@TranslationState {varToTemp} ->
    s {varToTemp = Map.insert varId currentTemp varToTemp, tmp = incTempLabel currentTemp}
  return currentTemp

-- Allocate stack slot for arguments (positive offsets from RBP)
allocateArgSlot :: Id -> Ty -> State TranslationState Int
allocateArgSlot varId ty = do
  currentOffset <- gets currentArgOffset
  let size = sizeOf ty
  let newOffset = currentOffset + size
  modify' $ \s@TranslationState {stackOffsets} ->
    s
      { stackOffsets = Map.insert varId currentOffset stackOffsets,
        currentArgOffset = newOffset
      }
  return currentOffset

-- Allocate stack slot for locals (negative offsets from RBP)
allocateLocalSlot :: Id -> Ty -> State TranslationState Int
allocateLocalSlot varId ty = do
  currentOffset <- gets currentLocalOffset
  let size = sizeOf ty
  let newOffset = currentOffset - size -- Grow downward
  modify' $ \s@TranslationState {stackOffsets} ->
    s
      { stackOffsets = Map.insert varId newOffset stackOffsets,
        currentLocalOffset = newOffset
      }
  return newOffset

-- Get the stack offset for a variable
getStackOffset :: Id -> State TranslationState (Maybe Int)
getStackOffset varId = gets (Map.lookup varId . stackOffsets)

idToStackOperand :: Id -> State TranslationState Operand
idToStackOperand varId = do
  -- check first if the variable is allocated on a temporary register
  maybeTemp <- gets (Map.lookup varId . varToTemp)
  case maybeTemp of
    Just temp -> return $ TempOperand temp
    Nothing -> do
      -- If not, check if it's allocated on the stack
      maybeOffset <- getStackOffset varId
      case maybeOffset of
        Just offset -> return $ StackOperand offset
        Nothing -> error $ "Variable " ++ varId ++ " not allocated on stack or temp register"

incTmp :: TranslationState -> TranslationState
incTmp ts@TranslationState {tmp} = ts {tmp = incTempLabel tmp}

incBlockId :: TranslationState -> TranslationState
incBlockId ts@TranslationState {blockId} = ts {blockId = blockId + 1}

addInstsToBlock :: [Inst] -> TranslationState -> TranslationState
addInstsToBlock insts ts@TranslationState {currentInsts = curInsts} =
  ts {currentInsts = curInsts ++ insts}

setCurBlockId :: BlockId -> TranslationState -> TranslationState
setCurBlockId blockId ts = ts {curBlockId = blockId}

cfgFromBlocks :: [BasicBlock] -> CFG
cfgFromBlocks blocks =
  let entryBlockId = case blocks of
        [] -> error "No blocks to create CFG"
        (BasicBlock {cfgBlockId} : _) -> cfgBlockId
      exitBlocks =
        foldr
          ( \(BasicBlock {blockTerminator}) acc -> case blockTerminator of
              (Return {}) -> acc ++ [entryBlockId]
              _ -> acc
          )
          mempty
          blocks
   in CFG {cfgEntryBlockId = entryBlockId, cfgExitBlocks = Set.fromList exitBlocks, cfgBlocks = blocks}

terminateBlock :: Terminator -> TranslationState -> TranslationState
terminateBlock terminator ts@TranslationState {blocks, currentInsts, curBlockId} =
  ts
    { blocks = BasicBlock {cfgBlockId = curBlockId, blockInsts = currentInsts, blockTerminator = terminator} : blocks,
      currentInsts = [],
      curBlockId = "terminated_"
    }

popBlocks :: TranslationState -> TranslationState
popBlocks ts = ts {blocks = []}

stackEnv :: Env.Env -> TranslationState -> TranslationState
stackEnv env ts@TranslationState {envs} =
  ts {envs = Env.pushEnv env envs}

popEnv :: TranslationState -> TranslationState
popEnv ts@TranslationState {envs} = ts {envs = Env.popEnv envs}

lookup :: Id -> TranslationState -> Maybe Env.Symbol
lookup id TranslationState {envs} = Env.lookup id envs

transExp :: Ast.TypedExp -> State TranslationState ()
transExp Ast.Exp {expAnnot = annot, expInner = exp}
  | Ast.IdExp {idName} <- exp = do
      t <- gets tmp
      stackOp <- idToStackOperand idName
      modify' $ addInstsToBlock [Assign {instDst = TempOperand t, instSrc = stackOp}]
  | Ast.NumberExp {numberValue} <- exp = do
      t <- gets tmp
      modify' $ addInstsToBlock [Assign {instDst = TempOperand t, instSrc = ConstInt numberValue}]
  | Ast.CharExp {charValue} <- exp = do
      t <- gets tmp
      modify' $ addInstsToBlock [Assign {instDst = TempOperand t, instSrc = ConstChar charValue}]
  | Ast.BinExp {binLeft = binLeft@Exp {expInner = binLeftInner}, binOp, binRight = binRight@Exp {expInner = binRightInner}} <- exp = do
      case (binLeftInner, binRightInner) of
        (Ast.NumberExp {numberValue = l}, Ast.NumberExp {numberValue = r}) -> do
          t <- gets tmp
          modify' $ addInstsToBlock [BinOp {instDst = TempOperand t, instBinop = binOp, instLeft = ConstInt l, instRight = ConstInt r}]
        (Ast.NumberExp {numberValue = l}, _) -> do
          transExp binRight
          t <- gets tmp
          modify' $ addInstsToBlock [BinOp {instDst = TempOperand t, instBinop = binOp, instLeft = ConstInt l, instRight = TempOperand t}]
        (_, Ast.NumberExp {numberValue = r}) -> do
          transExp binLeft
          t <- gets tmp
          modify' $ addInstsToBlock [BinOp {instDst = TempOperand t, instBinop = binOp, instLeft = TempOperand t, instRight = ConstInt r}]
        (Ast.CharExp {charValue = l}, Ast.CharExp {charValue = r}) -> do
          t <- gets tmp
          modify' $ addInstsToBlock [BinOp {instDst = TempOperand t, instBinop = binOp, instLeft = ConstChar l, instRight = ConstChar r}]
        (Ast.CharExp {charValue = l}, _) -> do
          transExp binRight
          t <- gets tmp
          modify' $ addInstsToBlock [BinOp {instDst = TempOperand t, instBinop = binOp, instLeft = ConstChar l, instRight = TempOperand t}]
        (_, Ast.CharExp {charValue = r}) -> do
          transExp binLeft
          t <- gets tmp
          modify' $ addInstsToBlock [BinOp {instDst = TempOperand t, instBinop = binOp, instLeft = TempOperand t, instRight = ConstChar r}]
        _ -> do
          transExp binLeft
          lt <- gets tmp
          modify' incTmp
          transExp binRight
          rt <- gets tmp
          modify' $ addInstsToBlock [BinOp {instDst = TempOperand rt, instBinop = binOp, instLeft = TempOperand lt, instRight = TempOperand rt}]
  | Ast.UnaryExp {unaryOp, unaryExp = Ast.Exp {expInner = Ast.NumberExp {numberValue}}} <- exp = do
      t <- gets tmp
      modify' $ addInstsToBlock [UnaryOp {instDst = TempOperand t, instUnop = unaryOp, instSrc = ConstInt numberValue}]
  | Ast.UnaryExp {unaryOp, unaryExp} <- exp = do
      transExp unaryExp
      t <- gets tmp
      modify' $ addInstsToBlock [UnaryOp {instDst = TempOperand t, instUnop = unaryOp, instSrc = TempOperand t}]
  | Ast.Call {callId, callArgs} <- exp = do
      mapM_
        ( \arg -> do
            transExp arg
            t <- gets tmp
            modify' incTmp
            modify' $ addInstsToBlock [Param {paramOperand = TempOperand t}]
        )
        callArgs
      case annot of
        VoidTy -> do
          modify' $ addInstsToBlock [Mir.Types.Call {callRet = Nothing, callFunId = callId, callArgCount = length callArgs}]
        _ -> do
          t <- gets tmp
          modify' $ addInstsToBlock [Mir.Types.Call {callRet = Just (TempOperand t), callFunId = callId, callArgCount = length callArgs}]
  | Ast.ArrAccess {arrId, arrIndex = Ast.Exp {expInner = Ast.NumberExp {numberValue}}} <- exp = do
      -- Accessing an array with a constant index
      let idx = ConstInt numberValue
      stackOp <- arrayAccess arrId idx
      t <- gets tmp
      modify' $ addInstsToBlock [Assign {instDst = TempOperand t, instSrc = stackOp}]
  | Ast.ArrAccess {arrId, arrIndex} <- exp = do
      transExp arrIndex
      t <- gets tmp
      stackOp <- arrayAccess arrId (TempOperand t)
      modify' $ addInstsToBlock [Assign {instDst = TempOperand t, instSrc = stackOp}]

arrayAccess :: Id -> Operand -> State TranslationState Operand
arrayAccess arrId (ConstInt idx) = do
  symb <- gets (lookup arrId)
  baseOffset <- getStackOffset arrId
  case (symb, baseOffset) of
    (Just Env.Symbol {Env.symbolTy = ArrTy {arrTyElemTy}}, Just offset) -> do
      let elemSize = sizeOf arrTyElemTy
      return $ StackOperand (offset + idx * elemSize)
    _ -> error $ "Array access error for: " ++ arrId
arrayAccess arrId (TempOperand idxTemp) = do
  symb <- gets (lookup arrId)
  baseOffset <- getStackOffset arrId
  case (symb, baseOffset) of
    (Just Env.Symbol {Env.symbolTy = ArrTy {arrTyElemTy}}, Just offset) -> do
      let elemSize = sizeOf arrTyElemTy
      -- Create a computed address temp
      t <- gets tmp
      modify' incTmp
      modify' $
        addInstsToBlock
          [ BinOp {instDst = TempOperand t, instBinop = TypeSystem.Mul, instLeft = TempOperand idxTemp, instRight = ConstInt elemSize},
            BinOp {instDst = TempOperand t, instBinop = TypeSystem.Add, instLeft = TempOperand t, instRight = ConstInt offset}
          ]
      return $ TempOperand t -- This temp holds the computed stack offset
    _ -> error $ "Array access error for: " ++ arrId
arrayAccess arrId _ = do
  -- This case should not happen, as we expect either ConstInt or Temp for index
  error $ "Invalid array access for: " ++ arrId

transStmt :: Ast.TypedStmt -> State TranslationState ()
transStmt stmt
  | Ast.ExpStmt {stmtExp} <- stmt = do
      transExp stmtExp
  | Ast.LetStmt {letVarDef = Ast.VarDef {varDefId, varDefTy}, letExp} <- stmt = do
      -- If the variable is a scalar allocate it on a temporary register
      -- If it's an array, allocate it on the stack
      symb <- gets (lookup varDefId)
      case symb of
        Just Env.Symbol {Env.symbolAlloc = Env.Local, Env.symbolTy = TypeSystem.IntTy} -> do
          transExp letExp
          t <- gets tmp -- Get the temp containing the expression result
          -- Treat variable as temp
          dstt <- allocateLocalToTmp varDefId
          modify' $ addInstsToBlock [Assign {instDst = TempOperand dstt, instSrc = TempOperand t}]
        _ -> do
          -- Allocate on stack
          _ <- allocateLocalSlot varDefId varDefTy
          transExp letExp
          t <- gets tmp
          stackOp <- idToStackOperand varDefId
          modify' (addInstsToBlock [Assign {instDst = stackOp, instSrc = TempOperand t}])
  | Ast.AssignStmt {assignId, assignExp} <- stmt = do
      transExp assignExp
      t <- gets tmp
      stackOp <- idToStackOperand assignId
      modify' (addInstsToBlock [Assign {instDst = stackOp, instSrc = TempOperand t}])
  | Ast.LetArrStmt {letArrVarDef = Ast.VarDef {varDefId}, letArrElems} <- stmt = do
      -- Store all the elements in the initializer in the array
      -- SInce the array is allocated on the stack, we can use the temporary
      -- registers to store the elements
      foldM_
        ( \idx elem -> do
            -- access the array and store the element
            dstOp <- arrayAccess varDefId (ConstInt idx)
            transExp elem
            tElem <- gets tmp
            modify' (addInstsToBlock [Assign {instDst = dstOp, instSrc = TempOperand tElem}])

            return (idx + 1)
        )
        0
        letArrElems
  | Ast.AssignArrStmt {assignArrId, assignArrIndex = Ast.Exp {expInner = Ast.NumberExp {numberValue}}, assignArrExp} <- stmt = do
      -- Assigning to an array with a constant index
      let idx = ConstInt numberValue
      dstOp <- arrayAccess assignArrId idx
      transExp assignArrExp
      tExp <- gets tmp
      modify' (addInstsToBlock [Assign {instDst = dstOp, instSrc = TempOperand tExp}])
  | Ast.AssignArrStmt {assignArrId, assignArrIndex, assignArrExp} <- stmt = do
      transExp assignArrIndex
      tIndex <- gets tmp
      modify' incTmp

      dstOp <- arrayAccess assignArrId (TempOperand tIndex)
      transExp assignArrExp
      tExp <- gets tmp
      modify' (addInstsToBlock [Assign {instDst = dstOp, instSrc = TempOperand tExp}])
  | Ast.ReturnStmt {returnExp} <- stmt = do
      case returnExp of
        Just exp -> do
          transExp exp
          t <- gets tmp
          modify' $ terminateBlock (Return {retOperand = Just (TempOperand t)})
        Nothing -> modify' $ terminateBlock (Return {retOperand = Nothing})
  | Ast.IfStmt {ifCond, ifBody, ifElseBody} <- stmt = do
      l <- gets blockId
      modify' incBlockId
      let thenBlockId = "IL" ++ show l
      transExp ifCond

      case ifElseBody of
        Just elseBody -> do
          l <- gets blockId
          modify' incBlockId
          let elseBlockId = "IL" ++ show l
          l <- gets blockId
          modify' incBlockId
          let endBlockId = "IL" ++ show l

          t <- gets tmp
          modify' $ terminateBlock CondJump {condOperand = TempOperand t, condTrueBlockId = thenBlockId, condFalseBlockId = elseBlockId}
          _ <- transBlock thenBlockId ifBody
          modify' $ terminateBlock Jump {jumpTarget = endBlockId}
          _ <- transBlock elseBlockId elseBody
          modify' $ terminateBlock Jump {jumpTarget = endBlockId}
          modify' (setCurBlockId endBlockId)
        Nothing -> do
          l <- gets blockId
          modify' incBlockId
          let endBlockId = "if_end_" ++ show l

          t <- gets tmp
          modify' $ terminateBlock CondJump {condOperand = TempOperand t, condTrueBlockId = thenBlockId, condFalseBlockId = endBlockId}
          _ <- transBlock thenBlockId ifBody
          modify' $ terminateBlock Jump {jumpTarget = endBlockId}
          modify' (setCurBlockId endBlockId)
  | Ast.WhileStmt {whileCond, whileBody} <- stmt = do
      l <- gets blockId
      modify' incBlockId
      let condBlockId = "WL" ++ show l
      l <- gets blockId
      modify' incBlockId
      let loopBlockId = "WL" ++ show l
      l <- gets blockId
      modify' incBlockId
      let endBlockId = "WL" ++ show l
      modify' incBlockId

      modify' $ terminateBlock Jump {jumpTarget = condBlockId}

      modify' (setCurBlockId condBlockId)
      transExp whileCond
      t <- gets tmp
      modify' $ terminateBlock CondJump {condOperand = TempOperand t, condTrueBlockId = loopBlockId, condFalseBlockId = endBlockId}

      _ <- transBlock loopBlockId whileBody
      modify' $ terminateBlock Jump {jumpTarget = condBlockId}

      modify' (setCurBlockId endBlockId)

transBlock :: String -> Ast.TypedBlock -> State TranslationState ()
transBlock blockId Ast.Block {blockAnnot = scope, blockStmts} = do
  modify' (setCurBlockId blockId)
  modify' (stackEnv scope)
  mapM_ transStmt blockStmts
  modify' popEnv

transFun :: Ast.TypedFun -> State TranslationState Mir.Types.Fun
transFun Ast.Fun {Ast.Types.funId, Ast.Types.funArgs, funBody} = do
  let Ast.Block {blockAnnot} = funBody

  -- Reset stack allocation for this function
  -- Args start at RBP + 16 (skip return addr + saved RBP)
  -- Locals start at RBP - 8 (first local below RBP)
  modify' $ \s ->
    s
      { tmp = Temp {tempLabel = 0},
        stackOffsets = Map.empty,
        varToTemp = Map.empty,
        currentArgOffset = 16, -- Start after saved RBP + return address
        currentLocalOffset = 0 -- Start at RBP, grow downward
      }

  -- Push the function's environment (which should contain the arguments)
  modify' (stackEnv blockAnnot)

  -- Now allocate stack space for arguments (they should already be in the env)
  args' <-
    mapM
      ( \VarDef {varDefId, varDefTy} -> do
          _ <- allocateArgSlot varDefId varDefTy -- Add to stackOffsets
          symb <- gets (lookup varDefId) -- Look up in environment
          case symb of
            Just s@Env.Symbol {Env.symbolAlloc = Env.Argument} -> return s
            _ -> error $ "Argument allocation error: " ++ varDefId
      )
      funArgs

  -- Allocate stack space for locals
  let locals =
        filter
          ( \Env.Symbol {Env.symbolAlloc} -> case symbolAlloc of
              Env.Local -> True
              _ -> False
          )
          (Env.toList blockAnnot)

  mapM_ (\Env.Symbol {Env.symbolId, Env.symbolTy} -> allocateLocalSlot symbolId symbolTy) locals

  -- Rest of function translation...
  transBlock funId funBody

  insts <- gets currentInsts
  unless (null insts) $
    modify' (terminateBlock Return {retOperand = Nothing})

  blockId <- gets curBlockId
  unless (blockId == "terminated_") $
    modify' (terminateBlock Return {retOperand = Nothing})

  cfg <- gets (cfgFromBlocks . reverse . blocks)

  modify' popBlocks

  return Mir.Types.Fun {Mir.Types.funId = funId, Mir.Types.funArgs = args', funLocals = locals, funCfg = cfg}

transExternFun :: Ast.ExternFun -> Mir.Types.ExternFun
transExternFun Ast.ExternFun {externFunId} = Mir.Types.ExternFun {externId = externFunId}

transProgram :: Ast.TypedProgram -> Mir.Types.Program
transProgram Ast.Program {programAnnot, programFuncs, Ast.Types.programExternFuns, Ast.Types.programMainFun} = do
  let externFuns' = transExternFun <$> programExternFuns
  let initialState =
        TranslationState
          { tmp = Temp {tempLabel = 0},
            blockId = 0,
            blocks = [],
            envs = Env.pushEnv programAnnot (Env.emptyEnvStack "global"),
            curBlockId = "global_entry",
            currentInsts = [],
            stackOffsets = mempty,
            currentLocalOffset = 0,
            currentArgOffset = 0,
            varToTemp = Map.empty
          }

  let (funs, st) =
        runState
          ( foldM
              ( \acc fun -> do
                  mirFun <- transFun fun
                  return (acc ++ [mirFun])
              )
              []
              programFuncs
          )
          initialState

  let mainFun' = case programMainFun of
        Just fun -> Just (runState (transFun fun) st)
        Nothing -> Nothing

  Mir.Types.Program {programFuns = funs, Mir.Types.programExternFuns = externFuns', Mir.Types.programMainFun = fst <$> mainFun'}

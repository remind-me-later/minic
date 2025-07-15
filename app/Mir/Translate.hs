module Mir.Translate (transProgram) where

import Ast qualified
import Ast.Types
import Control.Monad (foldM, foldM_, unless)
import Control.Monad.State (State, gets, modify', runState)
import Data.Set qualified as Set
import Mir.Types
import SymbolTable
import TypeSystem (BinOp (..), Id, Ty (..), sizeOf)

data TranslationState = TranslationState
  { tmp :: Temp,
    blockId :: Int,
    curBasicBlockId :: BasicBlockId,
    currentInsts :: [Inst],
    blocks :: [BasicBlock],
    symbolTable :: SymbolTable,
    curScopedBlockId :: BlockId
  }

idToStackOperand :: Id -> State TranslationState Operand
idToStackOperand varId = do
  -- check first if the variable is allocated on a temporary register
  blockId <- gets curScopedBlockId
  st <- gets symbolTable
  case getTempRegister varId blockId st of
    Just tempId -> return $ TempOperand (Temp {tempLabel = tempId})
    Nothing -> do
      -- If not, check if it's allocated on the stack
      case getStackOffset varId blockId st of
        Just offset -> return $ StackOperand offset
        Nothing ->
          case getStaticOffset varId blockId st of
            Just offset -> return $ DataOperand offset
            Nothing -> error $ "Variable " ++ varId ++ " not allocated"

incTmp :: TranslationState -> TranslationState
incTmp ts@TranslationState {tmp} = ts {tmp = incTempLabel tmp}

incBasicBlockId :: TranslationState -> TranslationState
incBasicBlockId ts@TranslationState {blockId} = ts {blockId = blockId + 1}

addInstsToBlock :: [Inst] -> TranslationState -> TranslationState
addInstsToBlock insts ts@TranslationState {currentInsts = curInsts} =
  ts {currentInsts = curInsts ++ insts}

setCurBasicBlockId :: BasicBlockId -> TranslationState -> TranslationState
setCurBasicBlockId blockId ts = ts {curBasicBlockId = blockId}

cfgFromBlocks :: [BasicBlock] -> CFG
cfgFromBlocks blocks =
  let entryBasicBlockId = case blocks of
        [] -> error "No blocks to create CFG"
        (BasicBlock {cfgBasicBlockId} : _) -> cfgBasicBlockId
      exitBlocks =
        foldr
          ( \(BasicBlock {blockTerminator}) acc -> case blockTerminator of
              (Return {}) -> acc ++ [entryBasicBlockId]
              _ -> acc
          )
          mempty
          blocks
   in CFG {cfgEntryBasicBlockId = entryBasicBlockId, cfgExitBlocks = Set.fromList exitBlocks, cfgBlocks = blocks}

terminateBlock :: Terminator -> TranslationState -> TranslationState
terminateBlock terminator ts@TranslationState {blocks, currentInsts, curBasicBlockId} =
  ts
    { blocks = BasicBlock {cfgBasicBlockId = curBasicBlockId, blockInsts = currentInsts, blockTerminator = terminator} : blocks,
      currentInsts = [],
      curBasicBlockId = "terminated_"
    }

popBlocks :: TranslationState -> TranslationState
popBlocks ts = ts {blocks = []}

popEnv :: TranslationState -> TranslationState
popEnv ts@TranslationState {symbolTable, curScopedBlockId} =
  ts {curScopedBlockId = prevEnv curScopedBlockId symbolTable}

lookupSymbolInState :: Id -> TranslationState -> Maybe Symbol
lookupSymbolInState identifier TranslationState {symbolTable, curScopedBlockId} =
  lookupSymbol identifier curScopedBlockId symbolTable

getStackOffsetInState :: Id -> TranslationState -> Maybe Int
getStackOffsetInState identifier TranslationState {symbolTable, curScopedBlockId} =
  getStackOffset identifier curScopedBlockId symbolTable

getStaticOffsetInState :: Id -> TranslationState -> Maybe Int
getStaticOffsetInState identifier TranslationState {symbolTable, curScopedBlockId} =
  getStaticOffset identifier curScopedBlockId symbolTable

transExp :: Ast.TypedExp -> State TranslationState ()
transExp Ast.Exp {expAnnot = annot, expInner}
  | Ast.IdExp {idName} <- expInner = do
      t <- gets tmp
      stackOp <- idToStackOperand idName
      modify' $ addInstsToBlock [Assign {instDst = TempOperand t, instSrc = stackOp}]
  | Ast.NumberExp {numberValue} <- expInner = do
      t <- gets tmp
      modify' $ addInstsToBlock [Assign {instDst = TempOperand t, instSrc = ConstInt numberValue}]
  | Ast.CharExp {charValue} <- expInner = do
      t <- gets tmp
      modify' $ addInstsToBlock [Assign {instDst = TempOperand t, instSrc = ConstChar charValue}]
  | Ast.BinExp {binLeft = binLeft@Exp {expInner = binLeftInner}, binOp, binRight = binRight@Exp {expInner = binRightInner}} <- expInner = do
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
  | Ast.UnaryExp {unaryOp, unaryExp = Ast.Exp {expInner = Ast.NumberExp {numberValue}}} <- expInner = do
      t <- gets tmp
      modify' $ addInstsToBlock [UnaryOp {instDst = TempOperand t, instUnop = unaryOp, instSrc = ConstInt numberValue}]
  | Ast.UnaryExp {unaryOp, unaryExp} <- expInner = do
      transExp unaryExp
      t <- gets tmp
      modify' $ addInstsToBlock [UnaryOp {instDst = TempOperand t, instUnop = unaryOp, instSrc = TempOperand t}]
  | Ast.Call {callId, callArgs} <- expInner = do
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
  | Ast.ArrAccess {arrId, arrIndex = Ast.Exp {expInner = Ast.NumberExp {numberValue}}} <- expInner = do
      -- Accessing an array with a constant index
      let idx = ConstInt numberValue
      stackOp <- arrayAccess arrId idx
      t <- gets tmp
      modify' $ addInstsToBlock [Assign {instDst = TempOperand t, instSrc = stackOp}]
  | Ast.ArrAccess {arrId, arrIndex} <- expInner = do
      transExp arrIndex
      t <- gets tmp
      stackOp <- arrayAccess arrId (TempOperand t)
      modify' $ addInstsToBlock [Assign {instDst = TempOperand t, instSrc = stackOp}]
  | Ast.TakeAddress {takeAddressId} <- expInner = do
      -- Take the address of a variable
      symb <- gets (lookupSymbolInState takeAddressId)
      case symb of
        Just Symbol {symbolStorage = Auto} -> do
          offset <- gets $ getStackOffsetInState takeAddressId
          case offset of
            Just off -> do
              t <- gets tmp
              modify' $ addInstsToBlock [Assign {instDst = TempOperand t, instSrc = StackOperand off}]
            Nothing -> error $ "Variable " ++ takeAddressId ++ " not allocated on stack"
        Just Symbol {symbolStorage = Static} -> do
          offset <- gets $ getStaticOffsetInState takeAddressId
          case offset of
            Just off -> do
              t <- gets tmp
              modify' $ addInstsToBlock [Assign {instDst = TempOperand t, instSrc = DataOperand off}]
            Nothing -> error $ "Static variable " ++ takeAddressId ++ " not found"
        _ -> error $ "Take address error for: " ++ takeAddressId

arrayAccess :: Id -> Operand -> State TranslationState Operand
arrayAccess arrId (ConstInt idx) = do
  blockId <- gets curScopedBlockId
  st <- gets symbolTable
  let symb = lookupSymbol arrId blockId st
  case symb of
    Just Symbol {symbolTy = ArrTy {arrTyElemTy}} -> do
      case getStackOffset arrId blockId st of
        Just offset -> do
          let elemSize = sizeOf arrTyElemTy
          return $ StackOperand (offset + idx * elemSize)
        Nothing -> error $ "Array " ++ arrId ++ " not allocated on stack"
    _ -> error $ "Array access error for: " ++ arrId
arrayAccess arrId (TempOperand idxTemp) = do
  blockId <- gets curScopedBlockId
  st <- gets symbolTable
  let symb = lookupSymbol arrId blockId st
  case symb of
    Just Symbol {symbolTy = ArrTy {arrTyElemTy}} -> do
      case getStackOffset arrId blockId st of
        Just offset -> do
          let elemSize = sizeOf arrTyElemTy
          -- Create a computed address temp
          t <- gets tmp
          modify' incTmp
          modify' $
            addInstsToBlock
              [ BinOp {instDst = TempOperand t, instBinop = Mul, instLeft = TempOperand idxTemp, instRight = ConstInt elemSize},
                BinOp {instDst = TempOperand t, instBinop = Add, instLeft = TempOperand t, instRight = ConstInt offset}
              ]
          return $ TempOperand t -- This temp holds the computed stack offset
        Nothing -> error $ "Array " ++ arrId ++ " not allocated on stack"
    _ -> error $ "Array access error for: " ++ arrId
arrayAccess arrId _ = do
  -- This case should not happen, as we expect either ConstInt or Temp for index
  error $ "Invalid array access for: " ++ arrId

transStmt :: Ast.TypedStmt -> State TranslationState ()
transStmt stmt
  | Ast.ExpStmt {stmtExp} <- stmt = do
      transExp stmtExp
  | Ast.LetStmt {letVarDef = Ast.VarDef {varDefId}, letExp} <- stmt = do
      -- If the variable is a scalar allocate it on a temporary register
      -- If it's an array, allocate it on the stack
      blockId <- gets curScopedBlockId
      st <- gets symbolTable
      let symb = lookupSymbol varDefId blockId st
      case symb of
        Just
          Symbol
            { symbolStorage = Auto,
              symbolTy = IntTy,
              addressTaken = False
            } -> do
            transExp letExp
            t <- gets tmp -- Get the temp containing the expression result
            -- Allocate temp register for the variable
            modify' $ \s -> s {symbolTable = allocateTempRegister varDefId blockId (symbolTable s)}
            stackOp <- idToStackOperand varDefId
            modify' $ addInstsToBlock [Assign {instDst = stackOp, instSrc = TempOperand t}]
        Just
          Symbol
            { symbolStorage = Static
            } -> do
            transExp letExp
            t <- gets tmp -- Get the temp containing the expression result
            -- Allocate static data slot
            modify' $ \s -> s {symbolTable = allocateStaticSlot varDefId blockId (symbolTable s)}
            stackOp <- idToStackOperand varDefId
            modify' $ addInstsToBlock [Assign {instDst = stackOp, instSrc = TempOperand t}]
        _ -> do
          -- Allocate on stack
          modify' $ \s -> s {symbolTable = allocateStackSlot varDefId blockId Auto (symbolTable s)}
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
        ( \idx item -> do
            -- access the array and store the element
            dstOp <- arrayAccess varDefId (ConstInt idx)
            transExp item
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
        Just expInner -> do
          transExp expInner
          t <- gets tmp
          modify' $ terminateBlock (Return {retOperand = Just (TempOperand t)})
        Nothing -> modify' $ terminateBlock (Return {retOperand = Nothing})
  | Ast.IfStmt {ifCond, ifBody, ifElseBody} <- stmt = do
      lthen <- gets blockId
      modify' incBasicBlockId
      let thenBasicBlockId = "IL" ++ show lthen
      transExp ifCond

      case ifElseBody of
        Just elseBody -> do
          lelse <- gets blockId
          modify' incBasicBlockId
          let elseBasicBlockId = "IL" ++ show lelse
          lend <- gets blockId
          modify' incBasicBlockId
          let endBasicBlockId = "IL" ++ show lend

          t <- gets tmp
          modify' $ terminateBlock CondJump {condOperand = TempOperand t, condTrueBasicBlockId = thenBasicBlockId, condFalseBasicBlockId = elseBasicBlockId}
          _ <- transBlock thenBasicBlockId ifBody
          modify' $ terminateBlock Jump {jumpTarget = endBasicBlockId}
          _ <- transBlock elseBasicBlockId elseBody
          modify' $ terminateBlock Jump {jumpTarget = endBasicBlockId}
          modify' (setCurBasicBlockId endBasicBlockId)
        Nothing -> do
          l <- gets blockId
          modify' incBasicBlockId
          let endBasicBlockId = "if_end_" ++ show l

          t <- gets tmp
          modify' $ terminateBlock CondJump {condOperand = TempOperand t, condTrueBasicBlockId = thenBasicBlockId, condFalseBasicBlockId = endBasicBlockId}
          _ <- transBlock thenBasicBlockId ifBody
          modify' $ terminateBlock Jump {jumpTarget = endBasicBlockId}
          modify' (setCurBasicBlockId endBasicBlockId)
  | Ast.WhileStmt {whileCond, whileBody} <- stmt = do
      lcond <- gets blockId
      modify' incBasicBlockId
      let condBasicBlockId = "WL" ++ show lcond
      lwhile <- gets blockId
      modify' incBasicBlockId
      let loopBasicBlockId = "WL" ++ show lwhile
      lend <- gets blockId
      modify' incBasicBlockId
      let endBasicBlockId = "WL" ++ show lend
      modify' incBasicBlockId

      modify' $ terminateBlock Jump {jumpTarget = condBasicBlockId}

      modify' (setCurBasicBlockId condBasicBlockId)
      transExp whileCond
      t <- gets tmp
      modify' $ terminateBlock CondJump {condOperand = TempOperand t, condTrueBasicBlockId = loopBasicBlockId, condFalseBasicBlockId = endBasicBlockId}

      _ <- transBlock loopBasicBlockId whileBody
      modify' $ terminateBlock Jump {jumpTarget = condBasicBlockId}

      modify' (setCurBasicBlockId endBasicBlockId)
  | Ast.ForStmt {forInit, forCond, forUpdate, forBody} <- stmt = do
      lcond <- gets blockId
      modify' incBasicBlockId
      let condBasicBlockId = "FL" ++ show lcond
      linc <- gets blockId
      modify' incBasicBlockId
      let loopBasicBlockId = "FL" ++ show linc
      lend <- gets blockId
      modify' incBasicBlockId
      let endBasicBlockId = "FL" ++ show lend
      modify' incBasicBlockId

      -- Add the initial statement to the block
      _ <- transStmt forInit

      modify' $ terminateBlock Jump {jumpTarget = condBasicBlockId}
      modify' (setCurBasicBlockId condBasicBlockId)
      transExp forCond
      t <- gets tmp
      modify' $ terminateBlock CondJump {condOperand = TempOperand t, condTrueBasicBlockId = loopBasicBlockId, condFalseBasicBlockId = endBasicBlockId}

      _ <- transBlock loopBasicBlockId forBody

      _ <- transStmt forUpdate
      modify' $ terminateBlock Jump {jumpTarget = condBasicBlockId}

      modify' (setCurBasicBlockId endBasicBlockId)

transBlock :: String -> Ast.TypedBlock -> State TranslationState ()
transBlock blockId Ast.Block {blockAnnot = scope, blockStmts} = do
  modify' (setCurBasicBlockId blockId)
  modify' (\s -> s {curScopedBlockId = scope})
  mapM_ transStmt blockStmts
  modify' popEnv

transFun :: Ast.TypedFun -> State TranslationState Mir.Types.Fun
transFun Ast.Fun {Ast.Types.funId, Ast.Types.funArgs, funBody} = do
  let Ast.Block {blockAnnot} = funBody

  -- Reset frame allocation for this function
  modify' $ \s -> s {symbolTable = resetFrameAllocation (symbolTable s)}

  -- Reset temp allocation for this function
  modify' $ \s ->
    s
      { tmp = Temp {tempLabel = 0}
      }

  -- Set the function's environment (which should contain the arguments)
  modify' (\s -> s {curScopedBlockId = blockAnnot})

  -- Now allocate stack space for arguments (they should already be in the env)
  args' <-
    mapM
      ( \VarDef {varDefId} -> do
          blockId <- gets curScopedBlockId
          modify' $ \s -> s {symbolTable = allocateStackSlot varDefId blockId Argument (symbolTable s)}
          st <- gets symbolTable
          case lookupSymbol varDefId blockId st of
            Just s@Symbol {symbolStorage = Argument} -> return s
            _ -> error $ "Argument allocation error: " ++ varDefId
      )
      funArgs

  blockId <- gets curScopedBlockId
  symbolTable' <- gets symbolTable

  -- Allocate stack space for locals
  -- FIXME: we already pass the symbol table to the x86 translator so this is useless
  let locals =
        filter
          ( \Symbol {symbolStorage} -> case symbolStorage of
              Auto -> True
              _ -> False
          )
          (toList blockId symbolTable')

  -- Rest of function translation...
  transBlock funId funBody

  insts <- gets currentInsts
  unless (null insts) $
    modify' (terminateBlock Return {retOperand = Nothing})

  curBlockId <- gets curBasicBlockId
  unless (curBlockId == "terminated_") $
    modify' (terminateBlock Return {retOperand = Nothing})

  cfg <- gets (cfgFromBlocks . reverse . blocks)

  modify' popBlocks

  return Mir.Types.Fun {Mir.Types.funId = funId, Mir.Types.funArgs = args', funLocals = locals, funCfg = cfg}

transExternFun :: Ast.ExternFun -> Mir.Types.ExternFun
transExternFun Ast.ExternFun {externFunId} = Mir.Types.ExternFun {externId = externFunId}

transProgram :: Ast.TypedProgram -> SymbolTable -> (Mir.Types.Program, SymbolTable)
transProgram Ast.Program {programAnnot, programFuncs, Ast.Types.programExternFuns, Ast.Types.programMainFun} symbolTable' =
  do
    let externFuns' = transExternFun <$> programExternFuns
    let initialState =
          TranslationState
            { tmp = Temp {tempLabel = 0},
              blockId = 0,
              blocks = [],
              symbolTable = symbolTable',
              curBasicBlockId = "global_entry",
              currentInsts = [],
              curScopedBlockId = programAnnot
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

    ( Mir.Types.Program
        { programFuns = funs,
          Mir.Types.programExternFuns = externFuns',
          Mir.Types.programMainFun = fst <$> mainFun'
        },
      case mainFun' of
        Just (_, st') -> symbolTable st'
        Nothing -> symbolTable st
      )

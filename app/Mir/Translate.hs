{-# LANGUAGE TemplateHaskell #-}

module Mir.Translate (transProgram) where

import Ast qualified
import Ast.Types
import Control.Lens
import Control.Monad (foldM, foldM_, unless)
import Control.Monad.State (State, runState)
import Data.Set qualified as Set
import Mir.Lenses
import Mir.Types
import SymbolTable
import TypeSystem (BinOp (..), Id, Ty (..), sizeOf)

data TranslationState = TranslationState
  { _tmp :: Temp,
    _blockId :: Int,
    _curBasicBlockId :: BasicBlockId,
    _currentInsts :: [Inst],
    _blocks :: [BasicBlock],
    _symbolTable :: SymbolTable,
    _curScopedBlockId :: BlockId
  }

makeLenses ''TranslationState

idToStackOperand :: Id -> State TranslationState Operand
idToStackOperand varId = do
  blockId' <- use curScopedBlockId
  st <- use symbolTable
  case getTempRegister varId blockId' st of
    Just tempId -> return $ TempOperand (Temp {_tempLabel = tempId})
    Nothing -> do
      case getStackOffset varId blockId' st of
        Just offset -> return $ StackOperand offset
        Nothing ->
          case getStaticOffset varId blockId' st of
            Just offset -> return $ DataOperand offset
            Nothing -> error $ "Variable " ++ varId ++ " not allocated"

incTmp :: State TranslationState ()
incTmp = tmp . tempLabel %= (+ 1)

incBasicBlockId :: State TranslationState ()
incBasicBlockId = blockId %= (+ 1)

addInstsToBlock :: [Inst] -> State TranslationState ()
addInstsToBlock insts = currentInsts %= (++ insts)

cfgFromBlocks :: [BasicBlock] -> CFG
cfgFromBlocks blocks' =
  let entryBasicBlockId = case blocks' of
        [] -> error "No blocks to create CFG"
        (BasicBlock {cfgBasicBlockId} : _) -> cfgBasicBlockId
      exitBlocks =
        foldr
          ( \(BasicBlock {blockTerminator}) acc -> case blockTerminator of
              (Return {}) -> acc ++ [entryBasicBlockId]
              _ -> acc
          )
          mempty
          blocks'
   in CFG {cfgEntryBasicBlockId = entryBasicBlockId, cfgExitBlocks = Set.fromList exitBlocks, cfgBlocks = blocks'}

terminateBlock :: Terminator -> State TranslationState ()
terminateBlock terminator = do
  curBlocks <- use blocks
  curInsts <- use currentInsts
  curBlockId <- use curBasicBlockId
  let newBlock = BasicBlock {cfgBasicBlockId = curBlockId, blockInsts = curInsts, blockTerminator = terminator}
  blocks .= newBlock : curBlocks
  currentInsts .= []
  curBasicBlockId .= "terminated_"

popBlocks :: State TranslationState ()
popBlocks = blocks .= []

popEnv :: State TranslationState ()
popEnv = do
  st <- use symbolTable
  curBlock <- use curScopedBlockId
  curScopedBlockId .= prevEnv curBlock st

lookupSymbolInState :: Id -> State TranslationState (Maybe Symbol)
lookupSymbolInState identifier = do
  st <- use symbolTable
  blockId' <- use curScopedBlockId
  return $ lookupSymbol identifier blockId' st

getStackOffsetInState :: Id -> State TranslationState (Maybe Int)
getStackOffsetInState identifier = do
  st <- use symbolTable
  blockId' <- use curScopedBlockId
  return $ getStackOffset identifier blockId' st

getStaticOffsetInState :: Id -> State TranslationState (Maybe Int)
getStaticOffsetInState identifier = do
  st <- use symbolTable
  blockId' <- use curScopedBlockId
  return $ getStaticOffset identifier blockId' st

transExp :: Ast.TypedExp -> State TranslationState ()
transExp Ast.Exp {_expAnnot = annot, _expInner}
  | Ast.IdExp {idName} <- _expInner = do
      t <- use tmp
      stackOp <- idToStackOperand idName
      addInstsToBlock [Assign {instDst = TempOperand t, instSrc = stackOp}]
  | Ast.NumberExp {numberValue} <- _expInner = do
      t <- use tmp
      addInstsToBlock [Assign {instDst = TempOperand t, instSrc = ConstInt numberValue}]
  | Ast.CharExp {charValue} <- _expInner = do
      t <- use tmp
      addInstsToBlock [Assign {instDst = TempOperand t, instSrc = ConstChar charValue}]
  | Ast.BinExp {binLeft = binLeft@Exp {_expInner = binLeftInner}, binOp, binRight = binRight@Exp {_expInner = binRightInner}} <- _expInner = do
      case (binLeftInner, binRightInner) of
        (Ast.NumberExp {numberValue = l}, Ast.NumberExp {numberValue = r}) -> do
          t <- use tmp
          addInstsToBlock [BinOp {instDst = TempOperand t, instBinop = binOp, instLeft = ConstInt l, instRight = ConstInt r}]
        (Ast.NumberExp {numberValue = l}, _) -> do
          transExp binRight
          t <- use tmp
          addInstsToBlock [BinOp {instDst = TempOperand t, instBinop = binOp, instLeft = ConstInt l, instRight = TempOperand t}]
        (_, Ast.NumberExp {numberValue = r}) -> do
          transExp binLeft
          t <- use tmp
          addInstsToBlock [BinOp {instDst = TempOperand t, instBinop = binOp, instLeft = TempOperand t, instRight = ConstInt r}]
        (Ast.CharExp {charValue = l}, Ast.CharExp {charValue = r}) -> do
          t <- use tmp
          addInstsToBlock [BinOp {instDst = TempOperand t, instBinop = binOp, instLeft = ConstChar l, instRight = ConstChar r}]
        (Ast.CharExp {charValue = l}, _) -> do
          transExp binRight
          t <- use tmp
          addInstsToBlock [BinOp {instDst = TempOperand t, instBinop = binOp, instLeft = ConstChar l, instRight = TempOperand t}]
        (_, Ast.CharExp {charValue = r}) -> do
          transExp binLeft
          t <- use tmp
          addInstsToBlock [BinOp {instDst = TempOperand t, instBinop = binOp, instLeft = TempOperand t, instRight = ConstChar r}]
        _ -> do
          transExp binLeft
          lt <- use tmp
          incTmp
          transExp binRight
          rt <- use tmp
          addInstsToBlock [BinOp {instDst = TempOperand rt, instBinop = binOp, instLeft = TempOperand lt, instRight = TempOperand rt}]
  | Ast.UnaryExp {unaryOp, unaryExp = Ast.Exp {_expInner = Ast.NumberExp {numberValue}}} <- _expInner = do
      t <- use tmp
      addInstsToBlock [UnaryOp {instDst = TempOperand t, instUnop = unaryOp, instSrc = ConstInt numberValue}]
  | Ast.UnaryExp {unaryOp, unaryExp} <- _expInner = do
      transExp unaryExp
      t <- use tmp
      addInstsToBlock [UnaryOp {instDst = TempOperand t, instUnop = unaryOp, instSrc = TempOperand t}]
  | Ast.Call {callId, callArgs} <- _expInner = do
      mapM_
        ( \arg -> do
            transExp arg
            t <- use tmp
            incTmp
            addInstsToBlock [Param {paramOperand = TempOperand t}]
        )
        callArgs
      case annot of
        VoidTy -> do
          addInstsToBlock [Mir.Types.Call {callRet = Nothing, callFunId = callId, callArgCount = length callArgs}]
        _ -> do
          t <- use tmp
          addInstsToBlock [Mir.Types.Call {callRet = Just (TempOperand t), callFunId = callId, callArgCount = length callArgs}]
  | Ast.ArrAccess {arrId, arrIndex = Ast.Exp {_expInner = Ast.NumberExp {numberValue}}} <- _expInner = do
      let idx = ConstInt numberValue
      stackOp <- arrayAccess arrId idx
      t <- use tmp
      addInstsToBlock [Assign {instDst = TempOperand t, instSrc = stackOp}]
  | Ast.ArrAccess {arrId, arrIndex} <- _expInner = do
      transExp arrIndex
      t <- use tmp
      stackOp <- arrayAccess arrId (TempOperand t)
      addInstsToBlock [Assign {instDst = TempOperand t, instSrc = stackOp}]
  | Ast.TakeAddress {takeAddressId} <- _expInner = do
      symb <- lookupSymbolInState takeAddressId
      case symb of
        Just Symbol {_symbolStorage = Auto} -> do
          offset <- getStackOffsetInState takeAddressId
          case offset of
            Just off -> do
              t <- use tmp
              addInstsToBlock [Assign {instDst = TempOperand t, instSrc = StackOperand off}]
            Nothing -> error $ "Variable " ++ takeAddressId ++ " not allocated on stack"
        Just Symbol {_symbolStorage = Static} -> do
          offset <- getStaticOffsetInState takeAddressId
          case offset of
            Just off -> do
              t <- use tmp
              addInstsToBlock [Assign {instDst = TempOperand t, instSrc = DataOperand off}]
            Nothing -> error $ "Static variable " ++ takeAddressId ++ " not found"
        _ -> error $ "Take address error for: " ++ takeAddressId

arrayAccess :: Id -> Operand -> State TranslationState Operand
arrayAccess arrId (ConstInt idx) = do
  blockId' <- use curScopedBlockId
  st <- use symbolTable
  let symb = lookupSymbol arrId blockId' st
  case symb of
    Just Symbol {_symbolTy = ArrTy {arrTyElemTy}} -> do
      case getStackOffset arrId blockId' st of
        Just offset -> do
          let elemSize = sizeOf arrTyElemTy
          return $ StackOperand (offset + idx * elemSize)
        Nothing -> error $ "Array " ++ arrId ++ " not allocated on stack"
    _ -> error $ "Array access error for: " ++ arrId
arrayAccess arrId (TempOperand idxTemp) = do
  blockId' <- use curScopedBlockId
  st <- use symbolTable
  let symb = lookupSymbol arrId blockId' st
  case symb of
    Just Symbol {_symbolTy = ArrTy {arrTyElemTy}} -> do
      case getStackOffset arrId blockId' st of
        Just offset -> do
          let elemSize = sizeOf arrTyElemTy
          t <- use tmp
          incTmp
          addInstsToBlock
            [ BinOp {instDst = TempOperand t, instBinop = Mul, instLeft = TempOperand idxTemp, instRight = ConstInt elemSize},
              BinOp {instDst = TempOperand t, instBinop = Add, instLeft = TempOperand t, instRight = ConstInt offset}
            ]
          return $ TempOperand t
        Nothing -> error $ "Array " ++ arrId ++ " not allocated on stack"
    _ -> error $ "Array access error for: " ++ arrId
arrayAccess arrId _ = error $ "Invalid array access for: " ++ arrId

transStmt :: Ast.TypedStmt -> State TranslationState ()
transStmt stmt
  | Ast.ExpStmt {stmtExp} <- stmt = transExp stmtExp
  | Ast.LetStmt {letVarDef = Ast.VarDef {_varDefId}, letExp} <- stmt = do
      blockId' <- use curScopedBlockId
      st <- use symbolTable
      let symb = lookupSymbol _varDefId blockId' st
      case symb of
        -- Just Symbol {_symbolStorage = Auto, _symbolTy = IntTy, _addressTaken = False} -> do
        --   transExp letExp
        --   t <- use tmp
        --   symbolTable %= allocateTempRegister _varDefId blockId'
        --   stackOp <- idToStackOperand _varDefId
        --   addInstsToBlock [Assign {instDst = stackOp, instSrc = TempOperand t}]
        Just Symbol {_symbolStorage = Static} -> do
          transExp letExp
          t <- use tmp
          symbolTable %= allocateStaticSlot _varDefId blockId'
          stackOp <- idToStackOperand _varDefId
          addInstsToBlock [Assign {instDst = stackOp, instSrc = TempOperand t}]
        _ -> do
          symbolTable %= allocateStackSlot _varDefId blockId' Auto
          transExp letExp
          t <- use tmp
          stackOp <- idToStackOperand _varDefId
          addInstsToBlock [Assign {instDst = stackOp, instSrc = TempOperand t}]
  | Ast.AssignStmt {assignId, assignExp} <- stmt = do
      transExp assignExp
      t <- use tmp
      stackOp <- idToStackOperand assignId
      addInstsToBlock [Assign {instDst = stackOp, instSrc = TempOperand t}]
  | Ast.LetArrStmt {letArrVarDef = Ast.VarDef {_varDefId}, letArrElems} <- stmt = do
      blockId' <- use curScopedBlockId
      st <- use symbolTable
      let symb = lookupSymbol _varDefId blockId' st
      case symb of
        Just Symbol {_symbolStorage = Static} -> do
          symbolTable %= allocateStaticSlot _varDefId blockId'
        _ -> symbolTable %= allocateStackSlot _varDefId blockId' Auto
      foldM_
        ( \idx item -> do
            dstOp <- arrayAccess _varDefId (ConstInt idx)
            transExp item
            tElem <- use tmp
            addInstsToBlock [Assign {instDst = dstOp, instSrc = TempOperand tElem}]
            return (idx + 1)
        )
        0
        letArrElems
  | Ast.AssignArrStmt {assignArrId, assignArrIndex = Ast.Exp {_expInner = Ast.NumberExp {numberValue}}, assignArrExp} <- stmt = do
      let idx = ConstInt numberValue
      dstOp <- arrayAccess assignArrId idx
      transExp assignArrExp
      tExp <- use tmp
      addInstsToBlock [Assign {instDst = dstOp, instSrc = TempOperand tExp}]
  | Ast.AssignArrStmt {assignArrId, assignArrIndex, assignArrExp} <- stmt = do
      transExp assignArrIndex
      tIndex <- use tmp
      incTmp
      dstOp <- arrayAccess assignArrId (TempOperand tIndex)
      transExp assignArrExp
      tExp <- use tmp
      addInstsToBlock [Assign {instDst = dstOp, instSrc = TempOperand tExp}]
  | Ast.ReturnStmt {returnExp} <- stmt = do
      case returnExp of
        Just _expInner -> do
          transExp _expInner
          t <- use tmp
          terminateBlock (Return {retOperand = Just (TempOperand t)})
        Nothing -> terminateBlock (Return {retOperand = Nothing})
  | Ast.IfStmt {ifCond, ifBody, ifElseBody} <- stmt = do
      lthen <- use blockId
      incBasicBlockId
      let thenBasicBlockId = "IL" ++ show lthen
      transExp ifCond

      case ifElseBody of
        Just elseBody -> do
          lelse <- use blockId
          incBasicBlockId
          let elseBasicBlockId = "IL" ++ show lelse
          lend <- use blockId
          incBasicBlockId
          let endBasicBlockId = "IL" ++ show lend

          t <- use tmp
          terminateBlock CondJump {condOperand = TempOperand t, condTrueBasicBlockId = thenBasicBlockId, condFalseBasicBlockId = elseBasicBlockId}
          transBlock thenBasicBlockId ifBody
          terminateBlock Jump {jumpTarget = endBasicBlockId}
          transBlock elseBasicBlockId elseBody
          terminateBlock Jump {jumpTarget = endBasicBlockId}
          curBasicBlockId .= endBasicBlockId
        Nothing -> do
          l <- use blockId
          incBasicBlockId
          let endBasicBlockId = "if_end_" ++ show l

          t <- use tmp
          terminateBlock CondJump {condOperand = TempOperand t, condTrueBasicBlockId = thenBasicBlockId, condFalseBasicBlockId = endBasicBlockId}
          transBlock thenBasicBlockId ifBody
          terminateBlock Jump {jumpTarget = endBasicBlockId}
          curBasicBlockId .= endBasicBlockId
  | Ast.WhileStmt {whileCond, whileBody} <- stmt = do
      lcond <- use blockId
      incBasicBlockId
      let condBasicBlockId = "WL" ++ show lcond
      lwhile <- use blockId
      incBasicBlockId
      let loopBasicBlockId = "WL" ++ show lwhile
      lend <- use blockId
      incBasicBlockId
      let endBasicBlockId = "WL" ++ show lend
      incBasicBlockId

      terminateBlock Jump {jumpTarget = condBasicBlockId}

      curBasicBlockId .= condBasicBlockId
      transExp whileCond
      t <- use tmp
      terminateBlock CondJump {condOperand = TempOperand t, condTrueBasicBlockId = loopBasicBlockId, condFalseBasicBlockId = endBasicBlockId}

      transBlock loopBasicBlockId whileBody
      terminateBlock Jump {jumpTarget = condBasicBlockId}

      curBasicBlockId .= endBasicBlockId
  | Ast.ForStmt {forInit, forCond, forUpdate, forBody} <- stmt = do
      lcond <- use blockId
      incBasicBlockId
      let condBasicBlockId = "FL" ++ show lcond
      linc <- use blockId
      incBasicBlockId
      let loopBasicBlockId = "FL" ++ show linc
      lend <- use blockId
      incBasicBlockId
      let endBasicBlockId = "FL" ++ show lend
      incBasicBlockId

      transStmt forInit

      terminateBlock Jump {jumpTarget = condBasicBlockId}
      curBasicBlockId .= condBasicBlockId
      transExp forCond
      t <- use tmp
      terminateBlock CondJump {condOperand = TempOperand t, condTrueBasicBlockId = loopBasicBlockId, condFalseBasicBlockId = endBasicBlockId}

      transBlock loopBasicBlockId forBody

      transStmt forUpdate
      terminateBlock Jump {jumpTarget = condBasicBlockId}

      curBasicBlockId .= endBasicBlockId

transBlock :: String -> Ast.TypedBlock -> State TranslationState ()
transBlock blockId' Ast.Block {Ast._blockId, _blockStmts} = do
  curBasicBlockId .= blockId'
  curScopedBlockId .= _blockId
  mapM_ transStmt _blockStmts
  popEnv

transFun :: Ast.TypedFun -> State TranslationState Mir.Types.Fun
transFun Ast.Fun {Ast.Types._funId, Ast.Types._funArgs, _funBody} = do
  let Ast.Block {Ast._blockId} = _funBody

  -- Reset frame allocation for this function
  symbolTable %= resetFrameAllocation

  -- Reset temp allocation for this function
  tmp .= Temp {_tempLabel = 0}

  -- Set the function's environment
  curScopedBlockId .= _blockId

  -- Allocate stack space for arguments
  args' <-
    mapM
      ( \VarDef {_varDefId} -> do
          blockId' <- use curScopedBlockId
          symbolTable %= allocateStackSlot _varDefId blockId' Argument
          st <- use symbolTable
          case lookupSymbol _varDefId blockId' st of
            Just s@Symbol {_symbolStorage = Argument} -> return s
            _ -> error $ "Argument allocation error: " ++ _varDefId
      )
      _funArgs

  blockId' <- use curScopedBlockId
  symbolTable' <- use symbolTable

  -- Get locals for the function
  let locals =
        filter
          ( \Symbol {_symbolStorage} -> case _symbolStorage of
              Auto -> True
              _ -> False
          )
          (toList blockId' symbolTable')

  transBlock _funId _funBody

  insts <- use currentInsts
  unless (null insts) $
    terminateBlock Return {retOperand = Nothing}

  curBlockId <- use curBasicBlockId
  unless (curBlockId == "terminated_") $
    terminateBlock Return {retOperand = Nothing}

  cfg <- uses blocks (cfgFromBlocks . reverse)

  popBlocks

  return Mir.Types.Fun {Mir.Types.funId = _funId, Mir.Types.funArgs = args', funLocals = locals, funCfg = cfg}

transExternFun :: Ast.ExternFun -> Mir.Types.ExternFun
transExternFun Ast.ExternFun {externFunId} = Mir.Types.ExternFun {externId = externFunId}

transProgram :: Ast.TypedProgram -> SymbolTable -> (Mir.Types.Program, SymbolTable)
transProgram Ast.Program {programFuncs, Ast.Types.programExternFuns, Ast.Types.programMainFun} symbolTable' = do
  let externFuns' = transExternFun <$> programExternFuns
  let initialState =
        TranslationState
          { _tmp = Temp {_tempLabel = 0},
            Mir.Translate._blockId = 0,
            _blocks = [],
            _symbolTable = symbolTable',
            _curBasicBlockId = "global_entry",
            _currentInsts = [],
            _curScopedBlockId = 0
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
      Just (_, st') -> _symbolTable st'
      Nothing -> _symbolTable st
    )

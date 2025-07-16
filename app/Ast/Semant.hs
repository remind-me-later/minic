{-# LANGUAGE TemplateHaskell #-}

module Ast.Semant
  ( typeProgram,
    TypedProgram,
    TypedFun,
    TypedBlock,
    TypedStmt,
    TypedExp,
  )
where

import Ast.Lenses
import Ast.Types
import Control.Lens
import Control.Monad (when)
import Control.Monad.State (State, runState)
import SymbolTable (BlockId, Symbol (..), SymbolTable)
import SymbolTable qualified
import TypeSystem

data TypingState = TypingState
  { _symbolTable :: SymbolTable,
    _currentBlockId :: BlockId,
    _errors :: [String],
    _curFun :: Maybe Id
  }

makeLenses ''TypingState

addErrorInState :: String -> State TypingState ()
addErrorInState err = errors %= (err :)

insertVarInState :: VarDef -> State TypingState ()
insertVarInState varDef = do
  curBlockId <- use currentBlockId
  symbolTable %= SymbolTable.insertVar varDef curBlockId

insertArgInState :: VarDef -> State TypingState ()
insertArgInState varDef = do
  curBlockId <- use currentBlockId
  symbolTable %= SymbolTable.insertArg varDef curBlockId

openEnvInState :: BlockId -> BlockId -> State TypingState ()
openEnvInState blockId' parentBlockId' = do
  st <- use symbolTable
  symbolTable .= SymbolTable.openEnv blockId' parentBlockId' st
  currentBlockId .= blockId'

closeEnvInState :: State TypingState ()
closeEnvInState = do
  st <- use symbolTable
  curBlock <- use currentBlockId
  currentBlockId .= SymbolTable.prevEnv curBlock st

lookupSymbolInState :: Id -> State TypingState (Maybe Symbol)
lookupSymbolInState identifier = do
  st <- use symbolTable
  curBlockId <- use currentBlockId
  return $ SymbolTable.lookupSymbol identifier curBlockId st

currentFunInState :: State TypingState (Maybe Symbol)
currentFunInState = do
  maybeFunId <- use curFun
  symbolTable' <- use symbolTable
  blockId' <- use currentBlockId
  return $ case maybeFunId of
    Nothing -> Nothing
    Just _funId -> SymbolTable.lookupSymbol _funId blockId' symbolTable'

setCurrentFunInState :: Id -> State TypingState ()
setCurrentFunInState = assign curFun . Just

typeBinOp :: BinOp -> TypedExp -> TypedExp -> State TypingState Ty
typeBinOp op' Exp {_expAnnot = lty} Exp {_expAnnot = rty}
  | (lty, rty) == (IntTy, IntTy) =
      case op' of
        Add -> return IntTy
        Sub -> return IntTy
        Mul -> return IntTy
        Div -> return IntTy
        Mod -> return IntTy
        Equal -> return BoolTy
        NotEqual -> return BoolTy
        LessThan -> return BoolTy
        GreaterThan -> return BoolTy
        LessThanOrEqual -> return BoolTy
        GreaterThanOrEqual -> return BoolTy
        _ -> do
          addErrorInState ("Unsupported operator for IntTy: " ++ show op')
          return VoidTy
  | (lty, rty) == (BoolTy, BoolTy) =
      case op' of
        And -> return BoolTy
        Or -> return BoolTy
        Xor -> return BoolTy
        Equal -> return BoolTy
        NotEqual -> return BoolTy
        _ -> do
          addErrorInState ("Unsupported operator for BoolTy: " ++ show op')
          return VoidTy
  | (lty, rty) == (CharTy, CharTy) =
      case op' of
        Add -> return CharTy
        Sub -> return CharTy
        Mul -> return CharTy
        Div -> return CharTy
        Equal -> return BoolTy
        NotEqual -> return BoolTy
        LessThan -> return BoolTy
        GreaterThan -> return BoolTy
        LessThanOrEqual -> return BoolTy
        GreaterThanOrEqual -> return BoolTy
        _ -> do
          addErrorInState ("Unsupported operator for CharTy: " ++ show op')
          return VoidTy
  | otherwise = do
      addErrorInState ("Type mismatch in operator: " ++ show lty ++ " " ++ show op' ++ " " ++ show rty)
      return VoidTy

typeUnaryOp :: UnaryOp -> TypedExp -> State TypingState Ty
typeUnaryOp op' Exp {_expAnnot}
  | IntTy <- _expAnnot =
      case op' of
        UnarySub -> return IntTy
        UnaryNot -> return BoolTy
        _ -> do
          addErrorInState ("Unsupported unary operator for IntTy: " ++ show op')
          return VoidTy
  | BoolTy <- _expAnnot =
      case op' of
        UnaryNot -> return BoolTy
        _ -> do
          addErrorInState ("Unsupported unary operator for BoolTy: " ++ show op')
          return VoidTy
  | CharTy <- _expAnnot =
      case op' of
        UnarySub -> return CharTy
        UnaryNot -> return BoolTy
        _ -> do
          addErrorInState ("Unsupported unary operator for CharTy: " ++ show op')
          return VoidTy
  | PtrTy {ptrTyElemTy} <- _expAnnot =
      case op' of
        UnaryPtrDeref -> return ptrTyElemTy
        _ -> do
          addErrorInState ("Can only dereference a pointer with '*', got: " ++ show _expAnnot)
          return VoidTy
  | otherwise = do
      addErrorInState ("Type mismatch in unary operator: " ++ show _expAnnot ++ " " ++ show op')
      return VoidTy

annotateIllegalExpAsVoid :: ExpInner Ty -> TypedExp
annotateIllegalExpAsVoid expInnerTyped =
  Exp
    { _expAnnot = VoidTy,
      _expInner = expInnerTyped
    }

typeExp :: RawExp -> State TypingState TypedExp
typeExp expression = case expression ^. expInner of
  IdExp {idName} -> do
    symb <- lookupSymbolInState idName
    case symb of
      Just VarSymbol {_varSymbolTy, _varSymbolStorage} -> do
        return
          Exp
            { _expAnnot = _varSymbolTy,
              _expInner = IdExp {idName}
            }
      Just ArgSymbol {_argSymbolTy, _argSymbolStorage} -> do
        return
          Exp
            { _expAnnot = _argSymbolTy,
              _expInner = IdExp {idName}
            }
      Just FunSymbol {_funSymbolTy} -> do
        addErrorInState ("Cannot use function " ++ idName ++ " as variable")
        return $ Exp {_expAnnot = _funSymbolTy, _expInner = IdExp {idName}}
      Nothing -> do
        addErrorInState ("Undefined variable: " ++ idName)
        return $ annotateIllegalExpAsVoid (IdExp {idName})
  NumberExp {numberValue} ->
    return
      Exp
        { _expAnnot = IntTy,
          _expInner = NumberExp {numberValue}
        }
  CharExp {charValue} ->
    return
      Exp
        { _expAnnot = CharTy,
          _expInner = CharExp {charValue}
        }
  BinExp {binLeft, binOp, binRight} -> do
    binLeft' <- typeExp binLeft
    binRight' <- typeExp binRight
    opTy <- typeBinOp binOp binLeft' binRight'
    return
      Exp
        { _expAnnot = opTy,
          _expInner = BinExp binLeft' binOp binRight'
        }
  UnaryExp {unaryOp, unaryExp} -> do
    unaryExp' <- typeExp unaryExp
    opTy <- typeUnaryOp unaryOp unaryExp'
    return Exp {_expAnnot = opTy, _expInner = UnaryExp unaryOp unaryExp'}
  Call {callId, callArgs} -> do
    symb <- lookupSymbolInState callId
    callArgs' <- mapM typeExp callArgs

    case symb of
      Just FunSymbol {_funSymbolTy = FunTy {funTyArgs, funTyRetTy}} -> do
        let argtys' = map (^. expAnnot) callArgs'
        when (argtys' /= funTyArgs) $
          addErrorInState
            ( "Argument type mismatch for function "
                ++ callId
                ++ ": expected "
                ++ show (length funTyArgs)
                ++ " arguments of types "
                ++ show funTyArgs
                ++ ", got "
                ++ show (length callArgs)
                ++ " arguments of types "
                ++ show argtys'
            )

        return
          Exp
            { _expAnnot = funTyRetTy,
              _expInner = Call {callId, callArgs = callArgs'}
            }
      Just FunSymbol {_funSymbolTy} -> do
        addErrorInState ("Internal error: expected function type, got " ++ show _funSymbolTy)
        return $ annotateIllegalExpAsVoid (Call {callId, callArgs = callArgs'})
      Just VarSymbol {_varSymbolTy, _varSymbolStorage} -> do
        addErrorInState ("Cannot call variable: " ++ callId ++ " of type: " ++ show _varSymbolTy)
        return $ annotateIllegalExpAsVoid (Call {callId, callArgs = callArgs'})
      Just ArgSymbol {_argSymbolTy, _argSymbolStorage} -> do
        addErrorInState ("Cannot call argument: " ++ callId ++ " of type: " ++ show _argSymbolTy)
        return $ annotateIllegalExpAsVoid (Call {callId, callArgs = callArgs'})
      Nothing -> do
        addErrorInState ("Undefined function: " ++ callId)
        return $ annotateIllegalExpAsVoid (Call {callId, callArgs = callArgs'})
  ArrAccess {arrId, arrIndex} -> do
    symb <- lookupSymbolInState arrId
    arrIndex' <- typeExp arrIndex
    let arrIndexTy = arrIndex' ^. expAnnot

    case symb of
      Just VarSymbol {_varSymbolTy = ArrTy {arrTyElemTy}} -> do
        when (arrIndexTy /= IntTy) $
          addErrorInState ("Array index must be of type IntTy, got " ++ show arrIndexTy)
        return
          Exp
            { _expAnnot = arrTyElemTy,
              _expInner = ArrAccess {arrId, arrIndex = arrIndex'}
            }
      Just VarSymbol {_varSymbolTy} -> do
        addErrorInState ("Cannot access array element of variable: " ++ arrId ++ " of type: " ++ show _varSymbolTy)
        return $ annotateIllegalExpAsVoid (ArrAccess {arrId, arrIndex = arrIndex'})
      -- TODO: passing arrays as arguments is not supported yet
      Just ArgSymbol {} -> do
        when (arrIndexTy /= IntTy) $
          addErrorInState ("Passing arrays as arguments is not supported yet, got index of type " ++ show arrIndexTy)
        return
          Exp
            { _expAnnot = VoidTy,
              _expInner = ArrAccess {arrId, arrIndex = arrIndex'}
            }
      Just FunSymbol {_funSymbolTy} -> do
        addErrorInState ("Cannot access array element of function: " ++ arrId ++ " of type: " ++ show _funSymbolTy)
        return $ annotateIllegalExpAsVoid (ArrAccess {arrId, arrIndex = arrIndex'})
      Nothing -> do
        addErrorInState ("Undefined variable: " ++ arrId)
        return $ annotateIllegalExpAsVoid (ArrAccess {arrId, arrIndex = arrIndex'})
  TakeAddress {takeAddressId} -> do
    symb <- lookupSymbolInState takeAddressId
    case symb of
      Just VarSymbol {_varSymbolTy, _varSymbolStorage} -> do
        return
          Exp
            { _expAnnot = PtrTy {ptrTyElemTy = _varSymbolTy},
              _expInner = TakeAddress {takeAddressId}
            }
      Just ArgSymbol {_argSymbolTy, _argSymbolStorage} -> do
        return
          Exp
            { _expAnnot = PtrTy {ptrTyElemTy = _argSymbolTy},
              _expInner = TakeAddress {takeAddressId}
            }
      Just FunSymbol {_funSymbolTy} -> do
        addErrorInState ("Cannot take address of function: " ++ takeAddressId ++ " of type: " ++ show _funSymbolTy)
        return $ annotateIllegalExpAsVoid (TakeAddress {takeAddressId})
      Nothing -> do
        addErrorInState ("Undefined variable: " ++ takeAddressId)
        return $ annotateIllegalExpAsVoid (TakeAddress {takeAddressId})

mkTypedLetStmt :: VarDef -> TypedExp -> Maybe StorageSpecifier -> TypedStmt
mkTypedLetStmt varDef expression storage = LetStmt {letVarDef = varDef, letExp = expression, letStorage = storage}

mkTypedLetArrStmt :: VarDef -> Int -> [TypedExp] -> Maybe StorageSpecifier -> TypedStmt
mkTypedLetArrStmt varDef size elems storage =
  LetArrStmt {letArrVarDef = varDef, letArrSize = size, letArrElems = elems, letArrStorage = storage}

mkTypedAssignStmt :: Id -> TypedExp -> TypedStmt
mkTypedAssignStmt identifier expression = AssignStmt {assignId = identifier, assignExp = expression}

mkTypedAssignArrStmt :: Id -> TypedExp -> TypedExp -> TypedStmt
mkTypedAssignArrStmt identifier arrayIndex expression = AssignArrStmt {assignArrId = identifier, assignArrIndex = arrayIndex, assignArrExp = expression}

mkTypedExpStmt :: TypedExp -> TypedStmt
mkTypedExpStmt expression = ExpStmt {stmtExp = expression}

mkTypedReturnStmt :: Maybe TypedExp -> TypedStmt
mkTypedReturnStmt expression = ReturnStmt {returnExp = expression}

mkTypedIfStmt :: TypedExp -> TypedBlock -> Maybe TypedBlock -> TypedStmt
mkTypedIfStmt cond body elseBody = IfStmt {ifCond = cond, ifBody = body, ifElseBody = elseBody}

mkTypedWhileStmt :: TypedExp -> TypedBlock -> TypedStmt
mkTypedWhileStmt cond body = WhileStmt {whileCond = cond, whileBody = body}

mkTypedForStmt :: TypedStmt -> TypedExp -> TypedStmt -> TypedBlock -> TypedStmt
mkTypedForStmt initializer condition update body =
  ForStmt {forInit = initializer, forCond = condition, forUpdate = update, forBody = body}

typeStmt :: RawStmt -> State TypingState TypedStmt
typeStmt stmt = case stmt of
  LetStmt {letVarDef, letExp, letStorage} -> do
    letExp' <- typeExp letExp
    let expTy = letExp' ^. expAnnot
        varTy = letVarDef ^. varDefTy

    when (expTy /= varTy) $
      addErrorInState ("Type mismatch in variable definition: expected " ++ show varTy ++ ", got " ++ show expTy)

    insertVarInState letVarDef

    return $ mkTypedLetStmt letVarDef letExp' letStorage
  LetArrStmt {letArrVarDef, letArrSize, letArrElems, letArrStorage} -> do
    letArrElems' <- mapM typeExp letArrElems

    let elemTypes = letArrElems' ^.. traverse . expAnnot
        varTy = letArrVarDef ^. varDefTy

    when (letArrSize <= 0) $
      addErrorInState ("Array size must be greater than 0, got: " ++ show letArrSize)

    when (any (/= varTy) elemTypes) $
      addErrorInState ("Type mismatch in array elements: expected " ++ show varTy ++ ", got " ++ show elemTypes)

    when (letArrSize /= length letArrElems) $
      addErrorInState ("Array size mismatch: expected " ++ show letArrSize ++ ", got " ++ show (length letArrElems))

    let arrDef = letArrVarDef & varDefTy .~ ArrTy {arrTyElemTy = varTy, arrTySize = letArrSize}
    insertVarInState arrDef
    return $ mkTypedLetArrStmt letArrVarDef letArrSize letArrElems' letArrStorage
  AssignArrStmt {assignArrId, assignArrIndex, assignArrExp} -> do
    symb <- lookupSymbolInState assignArrId
    assignArrIndex' <- typeExp assignArrIndex
    assignArrExp' <- typeExp assignArrExp

    let indexTy = assignArrIndex' ^. expAnnot
        expTy = assignArrExp' ^. expAnnot

    case symb of
      Just VarSymbol {_varSymbolTy = ArrTy {arrTyElemTy}} -> do
        when (indexTy /= IntTy) $
          addErrorInState ("Array index must be of type IntTy, got " ++ show indexTy)
        when (expTy /= arrTyElemTy) $
          addErrorInState ("Type mismatch in array assignment: expected " ++ show arrTyElemTy ++ ", got " ++ show expTy)
      Just VarSymbol {_varSymbolTy} -> do
        addErrorInState ("Cannot assign to non-array variable: " ++ assignArrId ++ " of type: " ++ show _varSymbolTy)
      Nothing ->
        addErrorInState ("Undefined variable: " ++ assignArrId)
      _ -> do
        addErrorInState ("Cannot assign to non-array variable: " ++ assignArrId)

    return $ mkTypedAssignArrStmt assignArrId assignArrIndex' assignArrExp'
  AssignStmt {assignId, assignExp} -> do
    symb <- lookupSymbolInState assignId
    assignExp' <- typeExp assignExp

    let expTy = assignExp' ^. expAnnot

    case symb of
      Just VarSymbol {_varSymbolTy, _varSymbolStorage} -> do
        when (expTy /= _varSymbolTy) $
          addErrorInState ("Type mismatch in assignment: expected " ++ show _varSymbolTy ++ ", got " ++ show expTy)
      Just ArgSymbol {_argSymbolTy, _argSymbolStorage} -> do
        when (expTy /= _argSymbolTy) $
          addErrorInState ("Type mismatch in assignment to argument: expected " ++ show _argSymbolTy ++ ", got " ++ show expTy)
      Just FunSymbol {_funSymbolTy} -> do
        addErrorInState ("Cannot assign to function: " ++ assignId ++ " of type: " ++ show _funSymbolTy)
      Nothing -> do
        addErrorInState ("Undefined variable: " ++ assignId)

    return $ mkTypedAssignStmt assignId assignExp'
  ExpStmt {stmtExp} -> do
    stmtExp' <- typeExp stmtExp
    return $ mkTypedExpStmt stmtExp'
  ReturnStmt {returnExp} -> do
    curfun <- currentFunInState
    (typedReturnExp, expty) <- case returnExp of
      Just expression -> do
        typedExp <- typeExp expression
        return (Just typedExp, typedExp ^. expAnnot)
      Nothing ->
        return (Nothing, VoidTy)

    case curfun of
      Just FunSymbol {_funSymbolTy = FunTy {funTyRetTy}} ->
        when (funTyRetTy /= expty) $
          addErrorInState ("Return type mismatch: expected " ++ show funTyRetTy ++ ", got " ++ show expty)
      Just FunSymbol {} ->
        addErrorInState "Internal error: expected function type, got a different symbol type"
      Just VarSymbol {} ->
        addErrorInState "Cannot return from a variable"
      Just ArgSymbol {} ->
        addErrorInState "Cannot return from an argument"
      Nothing ->
        addErrorInState "Unreachable: undefined function"

    return $ mkTypedReturnStmt typedReturnExp
  IfStmt {ifCond, ifBody, ifElseBody} -> do
    ifCond' <- typeExp ifCond
    let condTy = ifCond' ^. expAnnot

    when (condTy /= BoolTy) $
      addErrorInState ("Condition in if statement must be of type BoolTy, got " ++ show condTy)

    ifBody' <- typeBlock ifBody
    ifElseBody' <- traverse typeBlock ifElseBody

    return $ mkTypedIfStmt ifCond' ifBody' ifElseBody'
  WhileStmt {whileCond, whileBody} -> do
    whileCond' <- typeExp whileCond
    let condTy = whileCond' ^. expAnnot

    when (condTy /= BoolTy) $
      addErrorInState ("Condition in while statement must be of type BoolTy, got " ++ show condTy)

    whileBody' <- typeBlock whileBody
    return $ mkTypedWhileStmt whileCond' whileBody'
  ForStmt {forInit, forCond, forUpdate, forBody} -> do
    forInit' <- typeStmt forInit
    forCond' <- typeExp forCond
    forUpdate' <- typeStmt forUpdate
    forBody' <- typeBlock forBody

    let condTy = forCond' ^. expAnnot
    when (condTy /= BoolTy) $
      addErrorInState ("Condition in for statement must be of type BoolTy, got " ++ show condTy)

    return $ mkTypedForStmt forInit' forCond' forUpdate' forBody'

typeBlock :: RawBlock -> State TypingState TypedBlock
typeBlock Block {_blockStmts, _blockId} = do
  parentBlockId' <- use currentBlockId
  openEnvInState _blockId parentBlockId'
  annotatedStmts <- mapM typeStmt _blockStmts
  closeEnvInState
  return Block {_blockId, _blockStmts = annotatedStmts}

typeFun :: RawFun -> State TypingState TypedFun
typeFun Fun {_funId, _funArgs, _funRetTy, _funBody = Block {_blockStmts, _blockId}} = do
  parentBlockId' <- use currentBlockId
  openEnvInState _blockId parentBlockId'
  mapM_ insertArgInState _funArgs
  setCurrentFunInState _funId
  annotatedStmts <- mapM typeStmt _blockStmts
  closeEnvInState
  return Fun {_funId, _funArgs, _funRetTy, _funBody = Block {_blockId, _blockStmts = annotatedStmts}}

-- | No typing is performed for external functions since their types are assumed to be correct.
typeExternFun :: ExternFun -> State TypingState ExternFun
typeExternFun = return

typeProgram' :: RawProgram -> State TypingState TypedProgram
typeProgram' Program {programFuncs, programExternFuns, programMainFun} = do
  programFuncs' <- mapM typeFun programFuncs
  programExternFuns' <- mapM typeExternFun programExternFuns
  programMainFun' <- case programMainFun of
    Just f -> do
      typedMainFun <- typeFun f
      return (Just typedMainFun)
    Nothing -> do
      return Nothing

  return
    Program
      { programFuncs = programFuncs',
        programExternFuns = programExternFuns',
        programMainFun = programMainFun'
      }

buildGlobalSymbolTable :: RawProgram -> SymbolTable
buildGlobalSymbolTable Program {programFuncs, programExternFuns} =
  insertFunctions programFuncs $
    insertExternFunctions programExternFuns globalSymbolTable'
  where
    globalSymbolTable' = SymbolTable.globalSymbolTable
    blockId' = 0

    insertFunctions :: [RawFun] -> SymbolTable -> SymbolTable
    insertFunctions [] symbolTable' = symbolTable'
    insertFunctions (f : fs) symbolTable' =
      let funTy =
            FunTy
              { funTyArgs = map _varDefTy (_funArgs f),
                funTyRetTy = _funRetTy f
              }
          _funId' = _funId f
       in insertFunctions fs (SymbolTable.insertFunToEnv blockId' _funId' funTy symbolTable')

    insertExternFunctions :: [RawExternFun] -> SymbolTable -> SymbolTable
    insertExternFunctions [] symbolTable' = symbolTable'
    insertExternFunctions (ef : efs) symbolTable' =
      let efTy =
            FunTy
              { funTyArgs = externFunArgs ef,
                funTyRetTy = externFunRetTy ef
              }
          efId = externFunId ef
       in insertExternFunctions efs (SymbolTable.insertFunToEnv blockId' efId efTy symbolTable')

typeProgram :: RawProgram -> Either [String] (TypedProgram, SymbolTable)
typeProgram program =
  let initialState =
        TypingState
          { _symbolTable = buildGlobalSymbolTable program,
            _errors = [],
            _curFun = Nothing,
            _currentBlockId = 0
          }

      (typedProgram, finalState) = runState (typeProgram' program) initialState
   in if null (_errors finalState)
        then Right (typedProgram, _symbolTable finalState)
        else Left (_errors finalState)

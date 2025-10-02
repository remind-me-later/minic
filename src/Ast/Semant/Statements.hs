module Ast.Semant.Statements
  ( typeStmt,
    typeBlock,
  )
where

import Ast.Lenses
import Ast.Semant.Expressions
import Ast.Semant.State
import Ast.Types
import Control.Lens
import Control.Monad (when)
import Control.Monad.State (State)
import SymbolTable (Symbol (..))
import SymbolTable.Types (VarSymbolStorage (VarAutoStack, VarStatic))
import TypeSystem

-- Statement constructor helpers
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

-- | Type check a statement
typeStmt :: RawStmt -> State TypingState TypedStmt
typeStmt stmt = case stmt of
  LetStmt {letVarDef, letExp, letStorage} -> do
    letExp' <- typeExp letExp
    let expTy = letExp' ^. expAnnot
        varTy = letVarDef ^. varDefTy

    when (expTy /= varTy) $
      addErrorInState ("Type mismatch in variable definition: expected " ++ show varTy ++ ", got " ++ show expTy)

    case letStorage of
      Just Auto -> do
        insertVarInState letVarDef $ VarAutoStack 0
      Nothing -> do
        insertVarInState letVarDef $ VarAutoStack 0
      Just Static -> do
        insertVarInState letVarDef $ VarStatic 0
      Just Extern -> do
        -- unsupported for now
        addErrorInState "Internal error: Extern storage specifier is not supported in this context"

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

    case letArrStorage of
      Just Auto -> do
        insertVarInState arrDef $ VarAutoStack 0
      Nothing -> do
        insertVarInState arrDef $ VarAutoStack 0
      Just Static -> do
        insertVarInState arrDef $ VarStatic 0
      Just Extern -> do
        -- unsupported for now
        addErrorInState "Internal error: Extern storage specifier is not supported in this context"

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

-- | Type check a block
typeBlock :: RawBlock -> State TypingState TypedBlock
typeBlock Block {_blockStmts, _blockId} = do
  parentBlockId' <- use currentBlockId
  openEnvInState _blockId parentBlockId'
  annotatedStmts <- mapM typeStmt _blockStmts
  closeEnvInState
  return Block {_blockId, _blockStmts = annotatedStmts}

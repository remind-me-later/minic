module Ast.Semant
  ( typeProgram,
    TypedProgram,
    TypedFun,
    TypedBlock,
    TypedStmt,
    TypedExp,
  )
where

import Ast.Types
import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.State (State, gets, modify', runState)
import Data.Functor ((<&>))
import SymbolTable (Symbol (symbolTy))
import SymbolTable qualified
import TypeSystem

type TypedExp = Exp Ty

type TypedStmt = Stmt Ty SymbolTable.BlockId

type TypedBlock = Block Ty SymbolTable.BlockId

type TypedFun = Fun Ty SymbolTable.BlockId

type TypedProgram = Program Ty SymbolTable.BlockId

data TypingState = TypingState
  { symbolTable :: SymbolTable.SymbolTable,
    currentBlock :: SymbolTable.BlockId,
    errors :: [String],
    curFun :: Maybe Id
  }

addError :: String -> TypingState -> TypingState
addError err ts@TypingState {errors} =
  ts {errors = err : errors}

insertVar :: VarDef -> TypingState -> TypingState
insertVar v ts@TypingState {symbolTable, currentBlock} =
  ts {symbolTable = SymbolTable.insertVar v SymbolTable.Local currentBlock symbolTable}

insertArg :: VarDef -> TypingState -> TypingState
insertArg v ts@TypingState {symbolTable, currentBlock} =
  ts {symbolTable = SymbolTable.insertArg v currentBlock symbolTable}

openEnv :: TypingState -> TypingState
openEnv ts@TypingState {symbolTable} =
  let (newSymbolTable, currentBlock) = SymbolTable.openEnv symbolTable
   in ts {symbolTable = newSymbolTable, currentBlock}

closeEnv :: TypingState -> TypingState
closeEnv ts@TypingState {symbolTable, currentBlock} =
  ts {currentBlock = SymbolTable.prevEnv currentBlock symbolTable}

lookupSymbol :: Id -> TypingState -> Maybe SymbolTable.Symbol
lookupSymbol identifier TypingState {symbolTable, currentBlock} =
  SymbolTable.lookupSymbol identifier currentBlock symbolTable

currentFun :: TypingState -> Maybe SymbolTable.Symbol
currentFun TypingState {curFun, symbolTable, currentBlock} =
  curFun >>= \identifier -> SymbolTable.lookupSymbol identifier currentBlock symbolTable <|> Nothing

setCurrentFun :: Id -> TypingState -> TypingState
setCurrentFun identifier ts = ts {curFun = Just identifier}

typeBinOp :: BinOp -> TypedExp -> TypedExp -> State TypingState Ty
typeBinOp op Exp {expAnnot = lty} Exp {expAnnot = rty}
  | (lty, rty) == (IntTy, IntTy) =
      case op of
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
          modify' (addError ("Unsupported operator for IntTy: " ++ show op))
          return VoidTy
  | (lty, rty) == (BoolTy, BoolTy) =
      case op of
        And -> return BoolTy
        Or -> return BoolTy
        Xor -> return BoolTy
        Equal -> return BoolTy
        NotEqual -> return BoolTy
        _ -> do
          modify' (addError ("Unsupported operator for BoolTy: " ++ show op))
          return VoidTy
  | (lty, rty) == (CharTy, CharTy) =
      case op of
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
          modify' (addError ("Unsupported operator for CharTy: " ++ show op))
          return VoidTy
  | otherwise = do
      modify' (addError ("Type mismatch in operator: " ++ show lty ++ " " ++ show op ++ " " ++ show rty))
      return VoidTy

typeUnaryOp :: UnaryOp -> TypedExp -> State TypingState Ty
typeUnaryOp op Exp {expAnnot}
  | IntTy <- expAnnot =
      case op of
        UnarySub -> return IntTy
        UnaryNot -> return BoolTy
        UnaryPtrAddress -> return PtrTy {ptrTyElemTy = IntTy}
        _ -> do
          modify' (addError ("Unsupported unary operator for IntTy: " ++ show op))
          return VoidTy
  | BoolTy <- expAnnot =
      case op of
        UnaryNot -> return BoolTy
        UnaryPtrAddress -> return PtrTy {ptrTyElemTy = BoolTy}
        _ -> do
          modify' (addError ("Unsupported unary operator for BoolTy: " ++ show op))
          return VoidTy
  | CharTy <- expAnnot =
      case op of
        UnarySub -> return CharTy
        UnaryNot -> return BoolTy
        UnaryPtrAddress -> return PtrTy {ptrTyElemTy = CharTy}
        _ -> do
          modify' (addError ("Unsupported unary operator for CharTy: " ++ show op))
          return VoidTy
  | ptrTy@PtrTy {ptrTyElemTy} <- expAnnot =
      case op of
        UnaryPtrDeref -> return ptrTyElemTy
        UnaryPtrAddress -> return (PtrTy {ptrTyElemTy = ptrTy})
        _ -> do
          modify' (addError ("Can only dereference a pointer with '*', got: " ++ show expAnnot))
          return VoidTy
  | otherwise = do
      modify' (addError ("Type mismatch in unary operator: " ++ show expAnnot ++ " " ++ show op))
      return VoidTy

typeExp :: RawExp -> State TypingState TypedExp
typeExp Exp {expInner}
  | IdExp {idName} <- expInner = do
      symb <- gets (lookupSymbol idName)
      case symb of
        Just SymbolTable.Symbol {symbolTy = FunTy {funTyRetTy}} -> do
          modify' (addError ("Cannot use function " ++ idName ++ " as variable"))
          return Exp {expAnnot = funTyRetTy, expInner = IdExp {idName}}
        Just SymbolTable.Symbol {symbolTy} ->
          return Exp {expAnnot = symbolTy, expInner = IdExp {idName}}
        Nothing -> do
          modify' (addError ("Undefined variable: " ++ idName))
          return Exp {expAnnot = VoidTy, expInner = IdExp {idName}}
  | NumberExp {numberValue} <- expInner =
      return Exp {expAnnot = IntTy, expInner = NumberExp {numberValue}}
  | CharExp {charValue} <- expInner =
      return Exp {expAnnot = CharTy, expInner = CharExp {charValue}}
  | BinExp {binLeft, binOp, binRight} <- expInner = do
      binLeft' <- typeExp binLeft
      binRight' <- typeExp binRight
      opTy <- typeBinOp binOp binLeft' binRight'
      return Exp {expAnnot = opTy, expInner = BinExp {binLeft = binLeft', binOp, binRight = binRight'}}
  | UnaryExp {unaryOp, unaryExp} <- expInner = do
      unaryExp' <- typeExp unaryExp
      opTy <- typeUnaryOp unaryOp unaryExp'
      return Exp {expAnnot = opTy, expInner = UnaryExp {unaryOp, unaryExp = unaryExp'}}
  | Call {callId, callArgs} <- expInner = do
      symb <- gets (lookupSymbol callId)
      callArgs' <- mapM typeExp callArgs

      case symb of
        Just SymbolTable.Symbol {symbolTy = FunTy {funTyArgs, funTyRetTy}} -> do
          let argtys' = expAnnot <$> callArgs'
          when (argtys' /= funTyArgs) $
            modify'
              ( addError
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
              )

          return Exp {expAnnot = funTyRetTy, expInner = Call {callId, callArgs = callArgs'}}
        Just SymbolTable.Symbol {symbolTy} -> do
          modify' (addError ("Cannot call variable: " ++ callId ++ " of type: " ++ show symbolTy))
          return Exp {expAnnot = VoidTy, expInner = Call {callId, callArgs = callArgs'}}
        Nothing -> do
          modify' (addError ("Undefined function: " ++ callId))
          return Exp {expAnnot = VoidTy, expInner = Call {callId, callArgs = callArgs'}}
  | ArrAccess {arrId, arrIndex} <- expInner = do
      symb <- gets (lookupSymbol arrId)
      arrIndex'@Exp {expAnnot = arrIndexTy} <- typeExp arrIndex
      case symb of
        Just SymbolTable.Symbol {symbolTy = ArrTy {arrTyElemTy}} -> do
          when (arrIndexTy /= IntTy) $
            modify' (addError ("Array index must be of type IntTy, got " ++ show arrIndexTy))
          return Exp {expAnnot = arrTyElemTy, expInner = ArrAccess {arrId, arrIndex = arrIndex'}}
        Just SymbolTable.Symbol {symbolTy} -> do
          modify' (addError ("Cannot access array element of variable: " ++ arrId ++ " of type: " ++ show symbolTy))
          return Exp {expAnnot = VoidTy, expInner = ArrAccess {arrId, arrIndex = arrIndex'}}
        Nothing -> do
          modify' (addError ("Undefined variable: " ++ arrId))
          return Exp {expAnnot = VoidTy, expInner = ArrAccess {arrId, arrIndex = arrIndex'}}

typeStmt :: RawStmt -> State TypingState TypedStmt
typeStmt stmt
  | LetStmt {letVarDef = letVarDef@VarDef {varDefTy}, letExp} <- stmt = do
      letExp'@Exp {expAnnot} <- typeExp letExp

      when (expAnnot /= varDefTy) $
        modify' (addError ("Type mismatch in variable definition: expected " ++ show varDefTy ++ ", got " ++ show expAnnot))

      modify' $ insertVar letVarDef
      return LetStmt {letVarDef, letExp = letExp'}
  | LetArrStmt {letArrVarDef = letArrVarDef@VarDef {varDefTy}, letArrSize, letArrElems} <- stmt = do
      letArrElems' <- mapM typeExp letArrElems

      when (letArrSize <= 0) $
        modify' (addError ("Array size must be greater than 0, got: " ++ show letArrSize))

      let elemTypes = map expAnnot letArrElems'
      when (any (/= varDefTy) elemTypes) $
        modify' (addError ("Type mismatch in array elements: expected " ++ show varDefTy ++ ", got " ++ show elemTypes))

      let arrDef = letArrVarDef {varDefTy = ArrTy {arrTyElemTy = varDefTy, arrTySize = letArrSize}}

      when (letArrSize /= length letArrElems) $
        modify' (addError ("Array size mismatch: expected " ++ show letArrSize ++ ", got " ++ show (length letArrElems)))

      modify' $ insertVar arrDef
      return LetArrStmt {letArrVarDef, letArrSize, letArrElems = letArrElems'}
  | AssignArrStmt {assignArrId, assignArrIndex, assignArrExp} <- stmt = do
      symb <- gets (lookupSymbol assignArrId)
      assignArrIndex'@Exp {expAnnot} <- typeExp assignArrIndex
      assignArrExp' <- typeExp assignArrExp
      case symb of
        Just SymbolTable.Symbol {symbolTy = ArrTy {arrTyElemTy}} -> do
          when (expAnnot /= IntTy) $
            modify' (addError ("Array index must be of type IntTy, got " ++ show expAnnot))

          when (expAnnot /= arrTyElemTy) $
            modify' (addError ("Type mismatch in array assignment: expected " ++ show arrTyElemTy ++ ", got " ++ show expAnnot))
        Just SymbolTable.Symbol {symbolTy} -> do
          modify' (addError ("Cannot assign to non-array variable: " ++ assignArrId ++ " of type: " ++ show symbolTy))
        Nothing -> do
          modify' (addError ("Undefined variable: " ++ assignArrId))
      return AssignArrStmt {assignArrId, assignArrIndex = assignArrIndex', assignArrExp = assignArrExp'}
  | AssignStmt {assignId, assignExp} <- stmt = do
      symb <- gets (lookupSymbol assignId)
      assignExp'@Exp {expAnnot} <- typeExp assignExp

      case symb of
        Just SymbolTable.Symbol {symbolTy = FunTy {}} -> do
          modify' (addError ("Cannot assign to function: " ++ assignId))
        Just SymbolTable.Symbol {symbolTy} -> do
          when (expAnnot /= symbolTy) $
            modify' (addError ("Type mismatch in assignment: expected " ++ show symbolTy ++ ", got " ++ show expAnnot))
        Nothing -> do
          modify' (addError ("Undefined variable: " ++ assignId))

      return AssignStmt {assignId, assignExp = assignExp'}
  | ExpStmt {stmtExp} <- stmt = do
      stmtExp' <- typeExp stmtExp
      return ExpStmt {stmtExp = stmtExp'}
  | ReturnStmt {returnExp} <- stmt = do
      curfun <- gets currentFun
      (ret, expty) <- case returnExp of
        Just exp' -> do
          typedExp@Exp {expAnnot} <- typeExp exp'
          return (ReturnStmt {returnExp = Just typedExp}, expAnnot)
        Nothing -> return (ReturnStmt {returnExp = Nothing}, VoidTy)

      case curfun of
        Just SymbolTable.Symbol {symbolTy = FunTy {funTyRetTy}} ->
          when (funTyRetTy /= expty) $
            modify' (addError ("Return type mismatch: expected " ++ show funTyRetTy ++ ", got " ++ show expty))
        Just _ ->
          modify' (addError "Cannot return from a variable")
        Nothing ->
          modify' (addError "Unreachable: undefined function")

      return ret
  | IfStmt {ifCond, ifBody, ifElseBody} <- stmt = do
      ifCond'@Exp {expAnnot} <- typeExp ifCond

      when (expAnnot /= BoolTy) $
        modify' (addError ("Condition in if statement must be of type BoolTy, got " ++ show expAnnot))

      ifBody' <- typeBlock ifBody
      ifElseBody' <- case ifElseBody of
        Just elseBlock -> do
          typeBlock elseBlock <&> Just
        Nothing ->
          return Nothing

      return IfStmt {ifCond = ifCond', ifBody = ifBody', ifElseBody = ifElseBody'}
  | WhileStmt {whileCond, whileBody} <- stmt = do
      whileCond'@Exp {expAnnot} <- typeExp whileCond

      when (expAnnot /= BoolTy) $
        modify' (addError ("Condition in while statement must be of type BoolTy, got " ++ show expAnnot))

      whileBody' <- typeBlock whileBody
      return WhileStmt {whileCond = whileCond', whileBody = whileBody'}
  | ForStmt {forInit, forCond, forUpdate, forBody} <- stmt = do
      forInit' <- typeStmt forInit
      forCond'@Exp {expAnnot} <- typeExp forCond
      forUpdate' <- typeStmt forUpdate
      forBody' <- typeBlock forBody

      when (expAnnot /= BoolTy) $
        modify' (addError ("Condition in for statement must be of type BoolTy, got " ++ show expAnnot))

      return
        ForStmt
          { forInit = forInit',
            forCond = forCond',
            forUpdate = forUpdate',
            forBody = forBody'
          }

typeBlock :: RawBlock -> State TypingState TypedBlock
typeBlock Block {blockStmts} = do
  modify' openEnv
  blockId <- gets (\TypingState {symbolTable} -> SymbolTable.currentBlockId symbolTable)
  annotatedStmts <- mapM typeStmt blockStmts
  modify' closeEnv
  return Block {blockAnnot = blockId, blockStmts = annotatedStmts}

typeFun :: RawFun -> State TypingState TypedFun
typeFun Fun {funId, funArgs, funRetTy, funBody = Block {blockStmts}} = do
  modify' openEnv
  blockId <- gets (\TypingState {symbolTable} -> SymbolTable.currentBlockId symbolTable)
  mapM_ (modify' . insertArg) funArgs
  modify' (setCurrentFun funId)
  annotatedStmts <- mapM typeStmt blockStmts
  modify' closeEnv
  return Fun {funId, funArgs, funRetTy, funBody = Block {blockAnnot = blockId, blockStmts = annotatedStmts}}

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

  blockId <- gets (\TypingState {symbolTable} -> SymbolTable.currentBlockId symbolTable)

  return
    Program
      { programAnnot = blockId,
        programFuncs = programFuncs',
        programExternFuns = programExternFuns',
        programMainFun = programMainFun'
      }

buildGlobalEnv :: RawProgram -> SymbolTable.SymbolTable
buildGlobalEnv Program {programFuncs, programExternFuns} =
  insertFunctions programFuncs $
    insertExternFunctions programExternFuns globalSymbolTable
  where
    globalSymbolTable = SymbolTable.globalSymbolTable
    blockId = SymbolTable.currentBlockId globalSymbolTable

    insertFunctions :: [RawFun] -> SymbolTable.SymbolTable -> SymbolTable.SymbolTable
    insertFunctions [] symbolTable = symbolTable
    insertFunctions (f : fs) symbolTable =
      let funTy =
            FunTy
              { funTyArgs = map varDefTy (funArgs f),
                funTyRetTy = funRetTy f
              }
          funId' = funId f
       in insertFunctions fs (SymbolTable.insertFunToEnv blockId funId' funTy symbolTable)

    insertExternFunctions :: [RawExternFun] -> SymbolTable.SymbolTable -> SymbolTable.SymbolTable
    insertExternFunctions [] symbolTable = symbolTable
    insertExternFunctions (ef : efs) symbolTable =
      let efTy =
            FunTy
              { funTyArgs = externFunArgs ef,
                funTyRetTy = externFunRetTy ef
              }
          efId = externFunId ef
       in insertExternFunctions efs (SymbolTable.insertFunToEnv blockId efId efTy symbolTable)

typeProgram :: RawProgram -> Either [String] (TypedProgram, SymbolTable.SymbolTable)
typeProgram program =
  let initialState = makeInitialState program
      (typedProgram, finalState) = runState (typeProgram' program) initialState
   in if null (errors finalState)
        then Right (typedProgram, symbolTable finalState)
        else Left (errors finalState)

makeInitialState :: RawProgram -> TypingState
makeInitialState program =
  let globalEnv = buildGlobalEnv program
      currentBlockId = SymbolTable.currentBlockId globalEnv
   in TypingState
        { symbolTable = globalEnv,
          errors = [],
          curFun = Nothing,
          currentBlock = currentBlockId
        }

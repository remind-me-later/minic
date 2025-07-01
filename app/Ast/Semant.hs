{-# OPTIONS_GHC -Wno-name-shadowing #-}

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
import Env (Symbol (symbolTy))
import Env qualified
import TypeSystem
import Prelude hiding (lookup)

type TypedExp = Exp Ty

type TypedStmt = Stmt Ty Env.Env

type TypedBlock = Block Ty Env.Env

type TypedFun = Fun Ty Env.Env

type TypedProgram = Program Ty Env.Env

data TypingState = TypingState
  { envs :: Env.EnvStack,
    errors :: [String],
    curFun :: Maybe Id
  }

addError :: String -> TypingState -> TypingState
addError err ts@TypingState {errors} =
  ts {errors = err : errors}

insertVar :: VarDef -> TypingState -> TypingState
insertVar v ts@TypingState {envs} =
  ts {envs = Env.insertVar v envs}

insertArg :: VarDef -> TypingState -> TypingState
insertArg v ts@TypingState {envs} =
  ts {envs = Env.insertArg v envs}

pushEnv :: Env.Env -> TypingState -> TypingState
pushEnv env ts@TypingState {envs} = ts {envs = Env.pushEnv env envs}

popEnv :: TypingState -> TypingState
-- popEnv ts = ts {envs = Env.popEnv ts . envs}
popEnv ts@TypingState {envs} = ts {envs = Env.popEnv envs}

peekEnv :: TypingState -> Env.Env
peekEnv TypingState {envs} = Env.peekEnv envs

lookup :: Id -> TypingState -> Maybe Env.Symbol
lookup id TypingState {envs} = Env.lookup id envs

currentFun :: TypingState -> Maybe Env.Symbol
currentFun TypingState {curFun, envs} = curFun >>= \id -> Env.lookup id envs <|> Nothing

setCurrentFun :: Id -> TypingState -> TypingState
setCurrentFun id ts = ts {curFun = Just id}

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
  | expAnnot == IntTy =
      case op of
        UnarySub -> return IntTy
        _ -> do
          modify' (addError ("Unsupported unary operator for IntTy: " ++ show op))
          return VoidTy
  | expAnnot == BoolTy =
      case op of
        UnaryNot -> return BoolTy
        _ -> do
          modify' (addError ("Unsupported unary operator for BoolTy: " ++ show op))
          return VoidTy
  | expAnnot == CharTy =
      case op of
        UnarySub -> return CharTy
        _ -> do
          modify' (addError ("Unsupported unary operator for CharTy: " ++ show op))
          return VoidTy
  | otherwise = do
      modify' (addError ("Type mismatch in unary operator: " ++ show expAnnot ++ " " ++ show op))
      return VoidTy

typeExp :: RawExp -> State TypingState TypedExp
typeExp Exp {expInner}
  | IdExp {idName} <- expInner = do
      symb <- gets (lookup idName)
      case symb of
        Just Env.Symbol {symbolTy = FunTy {funTyRetTy}} -> do
          modify' (addError ("Cannot use function " ++ idName ++ " as variable"))
          return Exp {expAnnot = funTyRetTy, expInner = IdExp {idName}}
        Just Env.Symbol {symbolTy} ->
          return Exp {expAnnot = symbolTy, expInner = IdExp {idName}}
        Nothing -> do
          modify' (addError ("Undefined variable: " ++ idName))
          return Exp {expAnnot = VoidTy, expInner = IdExp {idName}}
  | NumberExp {numberValue} <- expInner =
      return Exp {expAnnot = IntTy, expInner = NumberExp {numberValue}}
  | CharExp {charValue} <- expInner =
      return Exp {expAnnot = CharTy, expInner = CharExp {charValue}}
  | BinExp {binLeft, binOp, binRight} <- expInner = do
      binLeft <- typeExp binLeft
      binRight <- typeExp binRight
      opTy <- typeBinOp binOp binLeft binRight
      return Exp {expAnnot = opTy, expInner = BinExp {binLeft, binOp, binRight}}
  | UnaryExp {unaryOp, unaryExp} <- expInner = do
      unaryExp <- typeExp unaryExp
      opTy <- typeUnaryOp unaryOp unaryExp
      return Exp {expAnnot = opTy, expInner = UnaryExp {unaryOp, unaryExp}}
  | Call {callId, callArgs} <- expInner = do
      symb <- gets (lookup callId)
      callArgs <- mapM typeExp callArgs

      case symb of
        Just Env.Symbol {symbolTy = FunTy {funTyArgs, funTyRetTy}} -> do
          let argtys' = expAnnot <$> callArgs
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

          return Exp {expAnnot = funTyRetTy, expInner = Call {callId, callArgs}}
        Just Env.Symbol {symbolTy} -> do
          modify' (addError ("Cannot call variable: " ++ callId ++ " of type: " ++ show symbolTy))
          return Exp {expAnnot = VoidTy, expInner = Call {callId, callArgs}}
        Nothing -> do
          modify' (addError ("Undefined function: " ++ callId))
          return Exp {expAnnot = VoidTy, expInner = Call {callId, callArgs}}
  | ArrAccess {arrId, arrIndex} <- expInner = do
      symb <- gets (lookup arrId)
      arrIndex@Exp {expAnnot = arrIndexTy} <- typeExp arrIndex
      case symb of
        Just Env.Symbol {symbolTy = ArrTy {arrTyElemTy}} -> do
          when (arrIndexTy /= IntTy) $
            modify' (addError ("Array index must be of type IntTy, got " ++ show arrIndexTy))
          return Exp {expAnnot = arrTyElemTy, expInner = ArrAccess {arrId, arrIndex}}
        Just Env.Symbol {symbolTy} -> do
          modify' (addError ("Cannot access array element of variable: " ++ arrId ++ " of type: " ++ show symbolTy))
          return Exp {expAnnot = VoidTy, expInner = ArrAccess {arrId, arrIndex}}
        Nothing -> do
          modify' (addError ("Undefined variable: " ++ arrId))
          return Exp {expAnnot = VoidTy, expInner = ArrAccess {arrId, arrIndex}}

typeStmt :: RawStmt -> State TypingState TypedStmt
typeStmt stmt
  | LetStmt {letVarDef = letVarDef@VarDef {varDefTy}, letExp} <- stmt = do
      letExp@Exp {expAnnot} <- typeExp letExp

      when (expAnnot /= varDefTy) $
        modify' (addError ("Type mismatch in variable definition: expected " ++ show varDefTy ++ ", got " ++ show expAnnot))

      modify' $ insertVar letVarDef
      return LetStmt {letVarDef, letExp}
  | LetArrStmt {letArrVarDef = letArrVarDef@VarDef {varDefTy}, letArrSize, letArrElems} <- stmt = do
      letArrElems <- mapM typeExp letArrElems

      when (letArrSize <= 0) $
        modify' (addError ("Array size must be greater than 0, got: " ++ show letArrSize))

      let elemTypes = map expAnnot letArrElems
      when (any (/= varDefTy) elemTypes) $
        modify' (addError ("Type mismatch in array elements: expected " ++ show varDefTy ++ ", got " ++ show elemTypes))

      let arrDef = letArrVarDef {varDefTy = ArrTy {arrTyElemTy = varDefTy, arrTySize = letArrSize}}

      when (letArrSize /= length letArrElems) $
        modify' (addError ("Array size mismatch: expected " ++ show letArrSize ++ ", got " ++ show (length letArrElems)))

      modify' $ insertVar arrDef
      return LetArrStmt {letArrVarDef, letArrSize, letArrElems}
  | AssignArrStmt {assignArrId, assignArrIndex, assignArrExp} <- stmt = do
      symb <- gets (lookup assignArrId)
      assignArrIndex@Exp {expAnnot} <- typeExp assignArrIndex
      assignArrExp <- typeExp assignArrExp
      case symb of
        Just Env.Symbol {symbolTy = ArrTy {arrTyElemTy}} -> do
          when (expAnnot /= IntTy) $
            modify' (addError ("Array index must be of type IntTy, got " ++ show expAnnot))

          when (expAnnot /= arrTyElemTy) $
            modify' (addError ("Type mismatch in array assignment: expected " ++ show arrTyElemTy ++ ", got " ++ show expAnnot))
        Just Env.Symbol {symbolTy} -> do
          modify' (addError ("Cannot assign to non-array variable: " ++ assignArrId ++ " of type: " ++ show symbolTy))
        Nothing -> do
          modify' (addError ("Undefined variable: " ++ assignArrId))
      return AssignArrStmt {assignArrId, assignArrIndex, assignArrExp}
  | AssignStmt {assignId, assignExp} <- stmt = do
      symb <- gets (lookup assignId)
      assignExp@Exp {expAnnot} <- typeExp assignExp

      case symb of
        Just Env.Symbol {symbolTy = FunTy {}} -> do
          modify' (addError ("Cannot assign to function: " ++ assignId))
        Just Env.Symbol {symbolTy} -> do
          when (expAnnot /= symbolTy) $
            modify' (addError ("Type mismatch in assignment: expected " ++ show symbolTy ++ ", got " ++ show expAnnot))
        Nothing -> do
          modify' (addError ("Undefined variable: " ++ assignId))

      return AssignStmt {assignId, assignExp}
  | ExpStmt {stmtExp} <- stmt = do
      stmtExp <- typeExp stmtExp
      return ExpStmt {stmtExp}
  | ReturnStmt {returnExp} <- stmt = do
      curfun <- gets currentFun
      (ret, expty) <- case returnExp of
        Just exp -> do
          typedExp@Exp {expAnnot} <- typeExp exp
          return (ReturnStmt {returnExp = Just typedExp}, expAnnot)
        Nothing -> return (ReturnStmt {returnExp = Nothing}, VoidTy)

      case curfun of
        Just Env.Symbol {symbolTy = FunTy {funTyRetTy}} ->
          when (funTyRetTy /= expty) $
            modify' (addError ("Return type mismatch: expected " ++ show funTyRetTy ++ ", got " ++ show expty))
        Just _ ->
          modify' (addError "Cannot return from a variable")
        Nothing ->
          modify' (addError "Unreachable: undefined function")

      return ret
  | IfStmt {ifCond, ifBody, ifElseBody} <- stmt = do
      ifCond@Exp {expAnnot} <- typeExp ifCond

      when (expAnnot /= BoolTy) $
        modify' (addError ("Condition in if statement must be of type BoolTy, got " ++ show expAnnot))

      ifBody <- typeBlock ifBody
      ifElseBody <- case ifElseBody of
        Just elseBlock -> do
          typeBlock elseBlock <&> Just
        Nothing ->
          return Nothing

      return IfStmt {ifCond, ifBody, ifElseBody}
  | WhileStmt {whileCond, whileBody} <- stmt = do
      whileCond@Exp {expAnnot} <- typeExp whileCond

      when (expAnnot /= BoolTy) $
        modify' (addError ("Condition in while statement must be of type BoolTy, got " ++ show expAnnot))

      whileBody <- typeBlock whileBody
      return WhileStmt {whileCond, whileBody}

typeBlock :: RawBlock -> State TypingState TypedBlock
typeBlock Block {blockStmts} = do
  modify' (pushEnv (Env.emptyEnv "block"))
  annotatedStmts <- mapM typeStmt blockStmts
  scope <- gets peekEnv
  modify' popEnv
  return Block {blockAnnot = scope, blockStmts = annotatedStmts}

typeFun :: RawFun -> State TypingState TypedFun
typeFun Fun {funId, funArgs, funRetTy, funBody = Block {blockStmts}} = do
  modify' (pushEnv $ Env.emptyEnv funId)
  mapM_ (modify' . insertArg) funArgs
  modify' (setCurrentFun funId)
  annotatedStmts <- mapM typeStmt blockStmts
  env <- gets peekEnv
  modify' popEnv
  return Fun {funId, funArgs, funRetTy, funBody = Block {blockAnnot = env, blockStmts = annotatedStmts}}

-- | No typing is performed for external functions since their types are assumed to be correct.
typeExternFun :: ExternFun -> State TypingState ExternFun
typeExternFun = return

typeProgram' :: RawProgram -> State TypingState TypedProgram
typeProgram' Program {programFuncs, programExternFuns, programMainFun} = do
  programFuncs <- mapM typeFun programFuncs
  programExternFuns <- mapM typeExternFun programExternFuns
  programMainFun <- case programMainFun of
    Just f -> do
      typedMainFun <- typeFun f
      return (Just typedMainFun)
    Nothing -> do
      return Nothing

  globalScope <- gets peekEnv

  return Program {programAnnot = globalScope, programFuncs, programExternFuns, programMainFun}

buildGlobalEnv :: RawProgram -> Env.EnvStack
buildGlobalEnv Program {programFuncs, programExternFuns} =
  insertFunctions programFuncs $
    insertExternFunctions programExternFuns $
      Env.emptyEnvStack "global"
  where
    insertFunctions [] env = env
    insertFunctions (f : fs) env = insertFunctions fs (Env.insertFun f env)

    insertExternFunctions [] env = env
    insertExternFunctions (ef : efs) env = insertExternFunctions efs (Env.insertExternFunction ef env)

typeProgram :: RawProgram -> Either [String] TypedProgram
typeProgram program =
  let initialState = makeInitialState program
      (typedProgram, finalState) = runState (typeProgram' program) initialState
   in if null (errors finalState)
        then Right typedProgram
        else Left (errors finalState)

makeInitialState :: RawProgram -> TypingState
makeInitialState program =
  TypingState
    { envs = buildGlobalEnv program,
      errors = [],
      curFun = Nothing
    }

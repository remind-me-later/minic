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
import Control.Monad (when)
import Control.Monad.State (State, gets, modify', runState)
import Data.Functor ((<&>))
import Env qualified
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
addError err ts = ts {errors = err : ts.errors}

insertVar :: VarDef -> TypingState -> TypingState
insertVar v ts = ts {envs = Env.insertVar v ts.envs}

insertArg :: VarDef -> TypingState -> TypingState
insertArg v ts = ts {envs = Env.insertArg v ts.envs}

pushEnv :: Env.Env -> TypingState -> TypingState
pushEnv env ts = ts {envs = Env.pushEnv env ts.envs}

popEnv :: TypingState -> TypingState
popEnv ts = ts {envs = Env.popEnv ts.envs}

peekEnv :: TypingState -> Env.Env
peekEnv ts = Env.peekEnv ts.envs

lookup :: Id -> TypingState -> Maybe Env.Symbol
lookup id ts = Env.lookup id ts.envs

currentFun :: TypingState -> Maybe Env.Symbol
currentFun ts = ts.curFun >>= flip Env.lookup ts.envs

setCurrentFun :: Id -> TypingState -> TypingState
setCurrentFun id ts = ts {curFun = Just id}

typeBinOp :: BinOp -> TypedExp -> TypedExp -> State TypingState Ty
typeBinOp op Exp {annot = lty} Exp {annot = rty}
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
  | otherwise = do
      modify' (addError ("Type mismatch in operator: " ++ show lty ++ " " ++ show op ++ " " ++ show rty))
      return VoidTy

typeUnaryOp :: UnaryOp -> TypedExp -> State TypingState Ty
typeUnaryOp op Exp {annot}
  | annot == IntTy =
      case op of
        UnarySub -> return IntTy
        _ -> do
          modify' (addError ("Unsupported unary operator for IntTy: " ++ show op))
          return VoidTy
  | annot == BoolTy =
      case op of
        UnaryNot -> return BoolTy
        _ -> do
          modify' (addError ("Unsupported unary operator for BoolTy: " ++ show op))
          return VoidTy
  | otherwise = do
      modify' (addError ("Type mismatch in unary operator: " ++ show annot ++ " " ++ show op))
      return VoidTy

typeExp :: RawExp -> State TypingState TypedExp
typeExp Exp {exp}
  | IdExp {id} <- exp = do
      symb <- gets (lookup id)
      case symb of
        Just Env.Symbol {ty = FunTy {retty}} -> do
          modify' (addError ("Cannot use function " ++ id ++ " as variable"))
          return Exp {annot = retty, exp = IdExp {id}}
        Just Env.Symbol {ty} ->
          return Exp {annot = ty, exp = IdExp {id}}
        Nothing -> do
          modify' (addError ("Undefined variable: " ++ id))
          return Exp {annot = VoidTy, exp = IdExp {id}}
  | NumberExp {num} <- exp =
      return Exp {annot = IntTy, exp = NumberExp {num}}
  | BinExp {left, op, right} <- exp = do
      left <- typeExp left
      right <- typeExp right
      opTy <- typeBinOp op left right
      return Exp {annot = opTy, exp = BinExp {left, op, right}}
  | UnaryExp {unop, exp} <- exp = do
      exp <- typeExp exp
      opTy <- typeUnaryOp unop exp
      return Exp {annot = opTy, exp = UnaryExp {unop, exp}}
  | Call {id, args = callargs} <- exp = do
      symb <- gets (lookup id)
      callargs <- mapM typeExp callargs

      case symb of
        Just Env.Symbol {ty = FunTy {args = funargs, retty}} -> do
          let argtys' = (.annot) <$> callargs
          when (argtys' /= funargs) $
            modify'
              ( addError
                  ( "Argument type mismatch for function "
                      ++ id
                      ++ ": expected "
                      ++ show (length funargs)
                      ++ " arguments of types "
                      ++ show funargs
                      ++ ", got "
                      ++ show (length callargs)
                      ++ " arguments of types "
                      ++ show argtys'
                  )
              )

          return Exp {annot = retty, exp = Call {id, args = callargs}}
        Just Env.Symbol {ty} -> do
          modify' (addError ("Cannot call variable: " ++ id ++ " of type: " ++ show ty))
          return Exp {annot = VoidTy, exp = Call {id, args = callargs}}
        Nothing -> do
          modify' (addError ("Undefined function: " ++ id))
          return Exp {annot = VoidTy, exp = Call {id, args = callargs}}
  | ArrAccess {id, index} <- exp = do
      symb <- gets (lookup id)
      index <- typeExp index
      case symb of
        Just Env.Symbol {ty = ArrTy {elemTy}} -> do
          when (index.annot /= IntTy) $
            modify' (addError ("Array index must be of type IntTy, got " ++ show index.annot))
          return Exp {annot = elemTy, exp = ArrAccess {id, index}}
        Just Env.Symbol {ty} -> do
          modify' (addError ("Cannot access array element of variable: " ++ id ++ " of type: " ++ show ty))
          return Exp {annot = VoidTy, exp = ArrAccess {id, index}}
        Nothing -> do
          modify' (addError ("Undefined variable: " ++ id))
          return Exp {annot = VoidTy, exp = ArrAccess {id, index}}

typeStmt :: RawStmt -> State TypingState TypedStmt
typeStmt stmt
  | LetStmt {vardef, exp} <- stmt = do
      exp <- typeExp exp

      when (exp.annot /= vardef.ty) $
        modify' (addError ("Type mismatch in variable definition: expected " ++ show vardef.ty ++ ", got " ++ show exp.annot))

      modify' $ insertVar vardef
      return LetStmt {vardef, exp}
  | LetArrStmt {vardef, size, elems} <- stmt = do
      elems <- mapM typeExp elems

      when (size <= 0) $
        modify' (addError ("Array size must be greater than 0, got: " ++ show size))

      let elemTypes = map (.annot) elems
      when (any (/= vardef.ty) elemTypes) $
        modify' (addError ("Type mismatch in array elements: expected " ++ show vardef.ty ++ ", got " ++ show elemTypes))

      let arrDef = vardef {ty = ArrTy {elemTy = vardef.ty, size}}

      when (size /= length elems) $
        modify' (addError ("Array size mismatch: expected " ++ show size ++ ", got " ++ show (length elems)))

      modify' $ insertVar arrDef
      return LetArrStmt {vardef, size, elems}
  | AssignArrStmt {id, index, exp} <- stmt = do
      symb <- gets (lookup id)
      index <- typeExp index
      exp <- typeExp exp
      case symb of
        Just Env.Symbol {ty = ArrTy {elemTy}} -> do
          when (index.annot /= IntTy) $
            modify' (addError ("Array index must be of type IntTy, got " ++ show index.annot))

          when (exp.annot /= elemTy) $
            modify' (addError ("Type mismatch in array assignment: expected " ++ show elemTy ++ ", got " ++ show exp.annot))
        Just Env.Symbol {ty} -> do
          modify' (addError ("Cannot assign to non-array variable: " ++ id ++ " of type: " ++ show ty))
        Nothing -> do
          modify' (addError ("Undefined variable: " ++ id))
      return AssignArrStmt {id, index, exp}
  | AssignStmt {id, exp} <- stmt = do
      symb <- gets (lookup id)
      exp <- typeExp exp

      case symb of
        Just Env.Symbol {ty = FunTy {}} -> do
          modify' (addError ("Cannot assign to function: " ++ id))
        Just Env.Symbol {ty} -> do
          when (exp.annot /= ty) $
            modify' (addError ("Type mismatch in assignment: expected " ++ show ty ++ ", got " ++ show exp.annot))
        Nothing -> do
          modify' (addError ("Undefined variable: " ++ id))

      return AssignStmt {id, exp}
  | ExpStmt {exp} <- stmt = do
      exp <- typeExp exp
      return ExpStmt {exp}
  | ReturnStmt {retexp} <- stmt = do
      curfun <- gets currentFun
      (ret, expty) <- case retexp of
        Just exp -> do
          typedExp <- typeExp exp
          return (ReturnStmt {retexp = Just typedExp}, typedExp.annot)
        Nothing -> return (ReturnStmt {retexp = Nothing}, VoidTy)

      case curfun of
        Just Env.Symbol {ty = FunTy {retty}} ->
          when (retty /= expty) $ modify' (addError ("Return type mismatch: expected " ++ show retty ++ ", got " ++ show expty))
        Just _ ->
          modify' (addError "Cannot return from a variable")
        Nothing ->
          modify' (addError "Unreachable: undefined function")

      return ret
  | IfStmt {cond, ifBody, elseBody} <- stmt = do
      cond <- typeExp cond

      when (cond.annot /= BoolTy) $
        modify' (addError ("Condition in if statement must be of type BoolTy, got " ++ show cond.annot))

      ifBody <- typeBlock ifBody
      elseBody <- case elseBody of
        Just elseBlock -> do
          typeBlock elseBlock <&> Just
        Nothing ->
          return Nothing

      return IfStmt {cond, ifBody, elseBody}
  | WhileStmt {cond, body} <- stmt = do
      cond <- typeExp cond

      when (cond.annot /= BoolTy) $
        modify' (addError ("Condition in while statement must be of type BoolTy, got " ++ show cond.annot))

      body <- typeBlock body
      return WhileStmt {cond, body}

typeBlock :: RawBlock -> State TypingState TypedBlock
typeBlock Block {stmts} = do
  modify' (pushEnv (Env.emptyEnv "block"))
  annotatedStmts <- mapM typeStmt stmts
  scope <- gets peekEnv
  modify' popEnv
  return Block {annot = scope, stmts = annotatedStmts}

typeFun :: RawFun -> State TypingState TypedFun
typeFun Fun {id, args, retty, body = Block {stmts}} = do
  modify' (pushEnv $ Env.emptyEnv id)
  mapM_ (modify' . insertArg) args
  modify' (setCurrentFun id)
  annotatedStmts <- mapM typeStmt stmts
  env <- gets peekEnv
  modify' popEnv
  return Fun {id, args, retty, body = Block {annot = env, stmts = annotatedStmts}}

-- | No typing is performed for external functions since their types are assumed to be correct.
typeExternFun :: ExternFun -> State TypingState ExternFun
typeExternFun = return

typeProgram' :: RawProgram -> State TypingState TypedProgram
typeProgram' Program {funcs, externFuns, mainFun} = do
  funcs <- mapM typeFun funcs
  externFuns <- mapM typeExternFun externFuns
  mainFun <- case mainFun of
    Just f -> do
      typedMainFun <- typeFun f
      return (Just typedMainFun)
    Nothing -> do
      return Nothing

  globalScope <- gets peekEnv

  return Program {annot = globalScope, funcs, externFuns, mainFun}

buildGlobalEnv :: RawProgram -> Env.EnvStack
buildGlobalEnv Program {funcs, externFuns} =
  insertFunctions funcs $
    insertExternFunctions externFuns $
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
   in if null finalState.errors
        then Right typedProgram
        else Left finalState.errors

makeInitialState :: RawProgram -> TypingState
makeInitialState program =
  TypingState
    { envs = buildGlobalEnv program,
      errors = [],
      curFun = Nothing
    }

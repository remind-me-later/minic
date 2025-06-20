{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
  ( Block (..),
    Exp (..),
    ExpInner (..),
    Fun (..),
    Id,
    Operator (..),
    Program (..),
    RawBlock,
    RawExp,
    RawFun,
    RawProgram,
    RawStmt,
    Stmt (..),
    Ty (..),
    VarDef (..),
  )
import Control.Monad (when)
import Control.Monad.State (State, gets, runState)
import Control.Monad.State.Lazy (modify)
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
    curFun :: Maybe Ast.Types.Id
  }

addError :: String -> TypingState -> TypingState
addError err ts = ts {errors = err : ts.errors}

insertVar :: VarDef -> TypingState -> TypingState
insertVar v ts@TypingState {envs} = ts {envs = Env.insertVar v envs}

insertArg :: VarDef -> TypingState -> TypingState
insertArg v ts@TypingState {envs} = ts {envs = Env.insertArg v envs}

pushEnv :: Env.Env -> TypingState -> TypingState
pushEnv env ts@TypingState {envs} = ts {envs = Env.pushEnv env envs}

popEnv :: TypingState -> TypingState
popEnv ts@TypingState {envs} = ts {envs = Env.popEnv envs}

peekEnv :: TypingState -> Env.Env
peekEnv TypingState {envs} = Env.peekEnv envs

lookup :: Ast.Types.Id -> TypingState -> Maybe Env.Symbol
lookup id TypingState {envs} = Env.lookup id envs

currentFun :: TypingState -> Maybe Env.Symbol
currentFun ts@TypingState {curFun} =
  case curFun of
    Just funId -> Env.lookup funId ts.envs
    Nothing -> Nothing

setCurrentFun :: Id -> TypingState -> TypingState
setCurrentFun funId ts = ts {curFun = Just funId}

typeOp :: Operator -> TypedExp -> TypedExp -> State TypingState Ty
typeOp op Exp {annot = lty} Exp {annot = rty} =
  case (lty, rty) of
    (IntTy, IntTy) -> case op of
      Add -> return IntTy
      Sub -> return IntTy
      Mul -> return IntTy
      Modulo -> return IntTy
      Equal -> return BoolTy
      NotEqual -> return BoolTy
      LessThan -> return BoolTy
      GreaterThan -> return BoolTy
      LessThanOrEqual -> return BoolTy
      GreaterThanOrEqual -> return BoolTy
      _ -> do
        modify (addError ("Unsupported operator for IntTy: " ++ show op))
        return VoidTy
    (BoolTy, BoolTy) -> case op of
      And -> return BoolTy
      Or -> return BoolTy
      Xor -> return BoolTy
      Not -> return BoolTy
      Equal -> return BoolTy
      NotEqual -> return BoolTy
      _ -> do
        modify (addError ("Unsupported operator for BoolTy: " ++ show op))
        return VoidTy
    _ ->
      do
        modify (addError ("Type mismatch in operator: " ++ show lty ++ " " ++ show op ++ " " ++ show rty))
        return VoidTy

typeExp :: RawExp -> State TypingState TypedExp
typeExp Exp {exp}
  | IdExp {id} <- exp = do
      symb <- gets (lookup id)
      case symb of
        Just Env.Symbol {ty = FunTy {retty}} -> do
          modify (addError ("Cannot use function " ++ id ++ " as variable"))
          return Exp {annot = retty, exp = IdExp {id}}
        Just Env.Symbol {ty} ->
          return Exp {annot = ty, exp = IdExp {id}}
        Nothing -> do
          modify (addError ("Undefined variable: " ++ id))
          return Exp {annot = VoidTy, exp = IdExp {id}}
  | NumberExp {num} <- exp =
      return Exp {annot = IntTy, exp = NumberExp {num}}
  | BinExp {left, op, right} <- exp = do
      left <- typeExp left
      right <- typeExp right
      opTy <- typeOp op left right
      return Exp {annot = opTy, exp = BinExp {left, op, right}}
  | Call {id, args = callargs} <- exp = do
      symb <- gets (lookup id)
      callargs <- mapM typeExp callargs

      case symb of
        Just Env.Symbol {ty = FunTy {args = funargs, retty}} -> do
          let argtys' = (\e -> e.annot) <$> callargs
          when (argtys' /= funargs) $
            modify
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
          modify (addError ("Cannot call variable: " ++ id ++ " of type: " ++ show ty))
          return Exp {annot = VoidTy, exp = Call {id, args = callargs}}
        Nothing -> do
          modify (addError ("Undefined function: " ++ id))
          return Exp {annot = VoidTy, exp = Call {id, args = callargs}}

typeStmt :: RawStmt -> State TypingState TypedStmt
typeStmt stmt
  | LetStmt {vardef = v@VarDef {ty}, exp} <- stmt = do
      exp <- typeExp exp

      when (exp.annot /= ty) $
        modify (addError ("Type mismatch in variable definition: expected " ++ show ty ++ ", got " ++ show exp.annot))

      modify $ insertVar v
      return LetStmt {vardef = v, exp}
  | AssignStmt {id, exp} <- stmt = do
      symb <- gets (lookup id)
      exp <- typeExp exp

      case symb of
        Just Env.Symbol {ty = FunTy {}} -> do
          modify (addError ("Cannot assign to function: " ++ id))
        Just Env.Symbol {ty} -> do
          when (exp.annot /= ty) $
            modify (addError ("Type mismatch in assignment: expected " ++ show ty ++ ", got " ++ show exp.annot))
        Nothing -> do
          modify (addError ("Undefined variable: " ++ id))

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
          when (retty /= expty) $ modify (addError ("Return type mismatch: expected " ++ show retty ++ ", got " ++ show expty))
        Just _ ->
          modify (addError "Cannot return from a variable")
        Nothing ->
          modify (addError "Unreachable: undefined function")

      return ret
  | IfStmt {cond, ifBody, elseBody} <- stmt = do
      cond <- typeExp cond

      when (cond.annot /= BoolTy) $
        modify (addError ("Condition in if statement must be of type BoolTy, got " ++ show cond.annot))

      ifBody <- typeBlock ifBody
      case elseBody of
        Just elseBlock -> do
          elseAnnot <- typeBlock elseBlock
          return IfStmt {cond, ifBody, elseBody = Just elseAnnot}
        Nothing ->
          return IfStmt {cond, ifBody, elseBody = Nothing}
  | WhileStmt {cond, body} <- stmt = do
      cond <- typeExp cond

      when (cond.annot /= BoolTy) $
        modify (addError ("Condition in while statement must be of type BoolTy, got " ++ show cond.annot))

      body <- typeBlock body
      return WhileStmt {cond, body}

typeBlock :: RawBlock -> State TypingState TypedBlock
typeBlock Block {stmts} = do
  modify (pushEnv (Env.emptyEnv "block"))
  annotatedStmts <- mapM typeStmt stmts
  scope <- gets peekEnv
  modify popEnv
  return Block {annot = scope, stmts = annotatedStmts}

typeFun :: RawFun -> State TypingState TypedFun
typeFun Fun {id, args, retty, body = Block {stmts}} = do
  modify (pushEnv $ Env.emptyEnv id)
  mapM_ (modify . insertArg) args
  modify (setCurrentFun id)
  annotatedStmts <- mapM typeStmt stmts
  env <- gets peekEnv
  modify popEnv
  return Fun {id, args, retty, body = Block {annot = env, stmts = annotatedStmts}}

typeProgram' :: RawProgram -> State TypingState TypedProgram
typeProgram' Program {funcs} = do
  funcs <- mapM typeFun funcs
  symb <- gets (lookup "main")
  case symb of
    Just Env.Symbol {ty = FunTy {retty}} ->
      when (retty /= VoidTy) (modify (addError "main function must return type Void"))
    Just _ -> modify (addError "main must be a function")
    Nothing -> return ()

  globalScope <- gets peekEnv

  return Program {annot = globalScope, funcs}

buildGlobalEnv :: RawProgram -> Env.EnvStack
buildGlobalEnv Program {funcs} =
  foldl (flip Env.insertFunction) (Env.emptyEnvStack "global") funcs

typeProgram :: RawProgram -> Either [String] TypedProgram
typeProgram program =
  let initialState = TypingState {envs = buildGlobalEnv program, errors = [], curFun = Nothing}
      (typedProgram, finalState) = runState (typeProgram' program) initialState
   in if null finalState.errors
        then Right typedProgram
        else Left finalState.errors

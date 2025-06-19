{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module TypeCheck
  ( typeProgram,
    TypedProgram,
    TypedFun,
    TypedBlock,
    TypedStmt,
    TypedExp,
  )
where

import Ast
  ( Block (..),
    Exp (..),
    ExpInner (..),
    Fun (..),
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
import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.State (MonadState (..), State, gets, runState)
import Control.Monad.State.Lazy (modify)
import Data.Map qualified as Map
import Env (openEnv)
import Env qualified

type TypedExp = Exp Ty

type TypedStmt = Stmt Ty Env.Env

type TypedBlock = Block Ty Env.Env

type TypedFun = Fun Ty Env.Env

type TypedProgram = Program Ty Env.Env

data TypingState = TypingState
  { envStack :: [Env.Env],
    errors :: [String]
  }

addError :: String -> TypingState -> TypingState
addError err ts =
  ts {errors = err : ts.errors}

insertVar :: VarDef -> TypingState -> TypingState
insertVar v ts@TypingState {envStack = head : tail} =
  ts {envStack = Env.insertVar v head : tail}
insertVar _ ts@TypingState {envStack = []} =
  ts {errors = "No scopes available to insert variable" : ts.errors}

pushEnv :: Env.Env -> TypingState -> TypingState
pushEnv newEnv ts@TypingState {envStack} =
  ts {envStack = newEnv : envStack}

popEnv :: TypingState -> TypingState
popEnv ts@TypingState {envStack = _ : tail} =
  ts {envStack = tail}
popEnv ts@TypingState {envStack = []} =
  ts {errors = "No scopes available to pop" : ts.errors}

peekEnv :: TypingState -> Env.Env
peekEnv TypingState {envStack = head : _} = head
peekEnv TypingState {envStack = []} =
  error "Unreachable: No scopes available to peek"

lookup :: String -> TypingState -> Maybe Env.Symbol
lookup id ts@TypingState {envStack = head : tail} =
  Env.lookup id head <|> case tail of
    [] -> Nothing
    _ -> TypeCheck.lookup id (ts {envStack = tail})
lookup _ TypingState {envStack = []} = Nothing

typeOp :: Operator -> Ty -> Ty -> Either String Ty
typeOp op lty rty =
  case op of
    Add
      | lty == IntTy && rty == IntTy -> Right IntTy
    Add -> Left $ "Type mismatch in addition: " ++ show lty ++ " + " ++ show rty
    Sub
      | lty == IntTy && rty == IntTy -> Right IntTy
    Sub -> Left $ "Type mismatch in subtraction: " ++ show lty ++ " - " ++ show rty
    Mul
      | lty == IntTy && rty == IntTy -> Right IntTy
    Mul -> Left $ "Type mismatch in multiplication: " ++ show lty ++ " * " ++ show rty
    Equal
      | lty == rty -> Right BoolTy
    Equal -> Left $ "Type mismatch in equality check: " ++ show lty ++ "== " ++ show rty
    NotEqual
      | lty == rty -> Right BoolTy
    NotEqual -> Left $ "Type mismatch in inequality check: " ++ show lty ++ " != " ++ show rty
    LessThan
      | lty == IntTy && rty == IntTy -> Right BoolTy
    LessThan -> Left $ "Type mismatch in less than check: " ++ show lty ++ " < " ++ show rty
    GreaterThan
      | lty == IntTy && rty == IntTy -> Right BoolTy
    GreaterThan -> Left $ "Type mismatch in greater than check: " ++ show lty ++ " > " ++ show rty
    LessThanOrEqual
      | lty == IntTy && rty == IntTy -> Right BoolTy
    LessThanOrEqual -> Left $ "Type mismatch in less than or equal check: " ++ show lty ++ " <= " ++ show rty
    GreaterThanOrEqual
      | lty == IntTy && rty == IntTy -> Right BoolTy
    GreaterThanOrEqual -> Left $ "Type mismatch in greater than or equal check: " ++ show lty ++ " >= " ++ show rty
    And
      | lty == BoolTy && rty == BoolTy -> Right BoolTy
    And -> Left $ "Type mismatch in logical AND: " ++ show lty ++ " && " ++ show rty
    Or
      | lty == BoolTy && rty == BoolTy -> Right BoolTy
    Or -> Left $ "Type mismatch in logical OR: " ++ show lty ++ "|| " ++ show rty
    Not
      | lty == BoolTy -> Right BoolTy
    Not -> Left $ "Type mismatch in logical NOT: expected BoolTy, got " ++ show lty
    Xor
      | lty == BoolTy && rty == BoolTy -> Right BoolTy
    Xor -> Left $ "Type mismatch in logical XOR: " ++ show lty ++ " ^ " ++ show rty
    Modulo
      | lty == IntTy && rty == IntTy -> Right IntTy
    Modulo -> Left $ "Type mismatch in modulo operation: " ++ show lty ++ " % " ++ show rty
    _ -> Left $ "Unsupported operator: " ++ show op

typeExp :: RawExp -> State TypingState TypedExp
typeExp Exp {exp}
  | IdExp {id} <- exp = do
      ts <- get
      case TypeCheck.lookup id ts of
        Just Env.Symbol {ty = FunTy {}} -> do
          modify (addError ("Cannot use function " ++ id ++ " as variable"))
          return Exp {annot = VoidTy, exp = IdExp {id}}
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
      let lty = left.annot
          rty = right.annot
      case typeOp op lty rty of
        Right ty -> return Exp {annot = ty, exp = BinExp {left, op, right}}
        Left err -> do
          modify (addError err)
          return Exp {annot = VoidTy, exp = BinExp {left, op, right}}
  | Call {id, args} <- exp = do
      ts <- get
      args <- mapM typeExp args

      case TypeCheck.lookup id ts of
        Just Env.Symbol {ty = FunTy {argtys, retty}} ->
          if length args /= length argtys
            then do
              modify
                ( addError
                    ( "Function "
                        ++ id
                        ++ " expects "
                        ++ show (length argtys)
                        ++ " arguments, got "
                        ++ show (length args)
                    )
                )
              return
                Exp {annot = VoidTy, exp = Call {id, args}}
            else do
              if ((\e -> e.annot) <$> args) == argtys
                then
                  return
                    Exp
                      { annot = retty,
                        exp = Call {id, args}
                      }
                else do
                  modify
                    ( addError
                        ( "Argument type mismatch for function "
                            ++ id
                            ++ ": expected "
                            ++ show argtys
                            ++ ", got "
                            ++ show (map (\e -> e.annot) args)
                        )
                    )
                  return Exp {annot = VoidTy, exp = Call {id, args}}
        Just Env.Symbol {ty} -> do
          -- Left $ "Cannot call variable of type: " ++ show ty ++ " as function: " ++ id
          modify (addError ("Cannot call variable: " ++ id ++ " of type: " ++ show ty))
          return Exp {annot = VoidTy, exp = Call {id, args}}
        Nothing -> do
          modify (addError ("Undefined function: " ++ id))
          return Exp {annot = VoidTy, exp = Call {id, args}}

typeStmt :: RawStmt -> State TypingState TypedStmt
typeStmt stmt
  | LetStmt {vardef = v@VarDef {ty}, exp} <- stmt = do
      exp <- typeExp exp
      let expTy = exp.annot
      if expTy == ty
        then do
          modify $ insertVar v
          return LetStmt {vardef = v, exp}
        else do
          modify (addError ("Type mismatch in let statement: expected " ++ show ty ++ ", got " ++ show expTy))
          return LetStmt {vardef = v, exp = Exp {annot = VoidTy, exp = IdExp {id = v.id}}}
  | AssignStmt {id, exp} <- stmt = do
      ts <- get
      case TypeCheck.lookup id ts of
        Just Env.Symbol {ty = FunTy {}} -> do
          modify (addError ("Cannot assign to function: " ++ id))
          return AssignStmt {id, exp = Exp {annot = VoidTy, exp = IdExp {id}}}
        Just Env.Symbol {ty} -> do
          exp <- typeExp exp
          let expty = exp.annot
          if expty == ty
            then
              return AssignStmt {id, exp}
            else do
              modify (addError ("Type mismatch in assignment: expected " ++ show ty ++ ", got " ++ show expty))
              return AssignStmt {id, exp = Exp {annot = VoidTy, exp = IdExp {id}}}
        Nothing -> do
          modify (addError ("Undefined variable: " ++ id))
          return AssignStmt {id, exp = Exp {annot = VoidTy, exp = IdExp {id}}}
  | ExpStmt {exp} <- stmt = do
      exp <- typeExp exp
      return ExpStmt {exp}
  | ReturnStmt {retexp} <- stmt = do
      ts <- get
      let curEnv = case envStack ts of
            [] -> error "Unreachable: No scopes available"
            (s : _) -> s
      case TypeCheck.lookup curEnv.name ts of
        Just Env.Symbol {ty = FunTy {retty}} -> do
          (right, expty) <- case retexp of
            Just exp -> do
              typedExp <- typeExp exp
              return (ReturnStmt {retexp = Just typedExp}, typedExp.annot)
            Nothing -> return (ReturnStmt {retexp = Nothing}, VoidTy)
          if retty == expty
            then
              return right
            else do
              modify
                ( addError
                    ( "Return expression type: "
                        ++ show expty
                        ++ " doesn't match function return type: "
                        ++ show retty
                    )
                )
              return right
        Just _ -> do
          modify (addError "Cannot return from a variable")
          return ReturnStmt {retexp = Nothing}
        Nothing -> do
          modify (addError "Unreachable: undefined function")
          return ReturnStmt {retexp = Nothing}
  | IfStmt {cond, ifBody, elseBody} <- stmt = do
      cond <- typeExp cond
      let condty = cond.annot

      when (condty /= BoolTy) $
        modify (addError ("Condition in if statement must be of type BoolTy, got " ++ show condty))

      ifBody <- typeBlock ifBody
      case elseBody of
        Just elseBlock -> do
          elseAnnot <- typeBlock elseBlock
          return IfStmt {cond, ifBody, elseBody = Just elseAnnot}
        Nothing ->
          return IfStmt {cond, ifBody, elseBody = Nothing}
  | WhileStmt {cond, body} <- stmt = do
      cond <- typeExp cond
      let condTy = cond.annot

      when (condTy /= BoolTy) $
        modify (addError ("Condition in while statement must be of type BoolTy, got " ++ show condTy))

      body <- typeBlock body
      return WhileStmt {cond, body}

typeBlock :: RawBlock -> State TypingState TypedBlock
typeBlock Block {stmts} = do
  let innerEnv = Env.openEnv "block"
  modify (pushEnv innerEnv)
  annotatedStmts <- mapM typeStmt stmts
  scope <- gets peekEnv
  modify popEnv
  return Block {annot = scope, stmts = reverse annotatedStmts}

typeFun :: RawFun -> State TypingState TypedFun
typeFun Fun {id, args, retty, body = Block {stmts}} = do
  let innerEnv = foldl (flip Env.insertArg) (Env.openEnv id) args
  modify (pushEnv innerEnv)
  annotatedStmts <- mapM typeStmt stmts
  env <- gets peekEnv
  let annotatedBody = Block {annot = env, stmts = reverse annotatedStmts}
  modify popEnv
  return Fun {id, args, retty, body = annotatedBody}

typeProgram' :: RawProgram -> State TypingState TypedProgram
typeProgram' Program {funcs} = do
  funcs <- mapM typeFun funcs
  ts <- get
  case TypeCheck.lookup "main" ts of
    Just Env.Symbol {ty = FunTy {retty}} -> do
      when (retty /= VoidTy) $
        modify (addError "Main function must return type Void")
    Just _ -> do
      modify (addError "Main function must be a function")
    Nothing -> return ()

  return Program {funcs}

buildGlobalEnv :: RawProgram -> Env.Env
buildGlobalEnv Program {funcs} =
  foldl
    (flip Env.insertFunction)
    Env.Env
      { name = "global",
        symbols = Map.empty
      }
    funcs

typeProgram :: RawProgram -> Either [String] TypedProgram
typeProgram program =
  let initialState = TypingState {envStack = [buildGlobalEnv program], errors = []}
      (typedProgram, finalState) = runState (typeProgram' program) initialState
   in if null finalState.errors
        then Right typedProgram
        else Left finalState.errors

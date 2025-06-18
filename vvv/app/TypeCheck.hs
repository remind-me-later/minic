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
import Control.Monad (foldM)
import Data.Map qualified as Map
import Scope qualified

type TypedExp = Exp Ty

type TypedStmt = Stmt Ty Scope.Scope

type TypedBlock = Block Ty Scope.Scope

type TypedFun = Fun Ty Scope.Scope

type TypedProgram = Program Ty Scope.Scope

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

typeExp :: Scope.Scope -> RawExp -> Either String TypedExp
typeExp curScope Exp {exp}
  | IdifierExp {id} <- exp =
      case Scope.lookup id curScope of
        Just Scope.Symbol {ty = FunTy {}} ->
          Left $ "Cannot use function " ++ id ++ " as variable"
        Just Scope.Symbol {ty} ->
          Right Exp {annot = ty, exp = IdifierExp {id}}
        Nothing -> Left $ "Undefined variable: " ++ id
  | NumberExp {num} <- exp =
      Right Exp {annot = IntTy, exp = NumberExp {num}}
  | BinExp {left, op, right} <- exp = do
      left <- typeExp curScope left
      right <- typeExp curScope right
      let lty = left.annot
          rty = right.annot
      case typeOp op lty rty of
        Right ty -> Right Exp {annot = ty, exp = BinExp {left, op, right}}
        Left err -> Left err
  | Call {id, args} <- exp =
      case Scope.lookup id curScope of
        Just Scope.Symbol {ty = FunTy {argtys, retty}} ->
          if length args /= length argtys
            then
              Left $
                "Function "
                  ++ id
                  ++ " expects "
                  ++ show (length argtys)
                  ++ " arguments, got "
                  ++ show (length args)
            else do
              args <- mapM (typeExp curScope) args
              if ((\e -> e.annot) <$> args) == argtys
                then
                  Right
                    Exp
                      { annot = retty,
                        exp = Call {id, args}
                      }
                else
                  Left $
                    "Argument type mismatch for function "
                      ++ id
                      ++ ": expected "
                      ++ show argtys
                      ++ ", got "
                      ++ show args
        Just Scope.Symbol {ty} ->
          Left $ "Cannot call variable of type: " ++ show ty ++ " as function: " ++ id
        Nothing -> Left $ "Undefined function: " ++ id

typeStmt :: Scope.Scope -> RawStmt -> Either String (TypedStmt, Scope.Scope)
typeStmt curScope stmt
  | LetStmt {vardef = v@VarDef {ty}, exp} <- stmt = do
      exp <- typeExp curScope exp
      let expTy = exp.annot
      if expTy == ty
        then
          Right
            ( LetStmt
                { vardef = v,
                  exp
                },
              Scope.insertVar v curScope
            )
        else Left $ "Type mismatch: expected " ++ show ty ++ ", got " ++ show expTy
  | AssignStmt {id, exp} <- stmt = do
      case Scope.lookup id curScope of
        Just Scope.Symbol {ty = FunTy {}} -> Left $ "Cannot assign to function: " ++ id
        Just Scope.Symbol {ty} -> do
          exp <- typeExp curScope exp
          let expty = exp.annot
          if expty == ty
            then
              Right (AssignStmt {id, exp}, curScope)
            else Left $ "Type mismatch in assignment: expected " ++ show ty ++ ", got " ++ show expty
        Nothing -> Left $ "Undefined variable: " ++ id
  | ExpStmt {exp} <- stmt = do
      exp <- typeExp curScope exp
      Right (ExpStmt {exp}, curScope)
  | ReturnStmt {retexp} <- stmt = do
      case Scope.lookup curScope.name curScope of
        Just Scope.Symbol {ty = FunTy {retty}} -> do
          (right, expty) <- case retexp of
            Just exp -> do
              typedExp <- typeExp curScope exp
              return (ReturnStmt {retexp = Just typedExp}, typedExp.annot)
            Nothing -> return (ReturnStmt {retexp = Nothing}, VoidTy)
          if retty == expty
            then
              Right (right, curScope)
            else
              Left $
                "Return expession type: "
                  ++ show expty
                  ++ " doesn't match function return type: "
                  ++ show retty
        Just _ -> Left "Cannot return from a variable"
        Nothing -> Left "unreachable: undefined function"
  | IfStmt {cond, ifBody, elseBody} <- stmt = do
      cond <- typeExp curScope cond
      let condty = cond.annot
      if condty == BoolTy
        then do
          ifBody <- typeBlock curScope ifBody
          case elseBody of
            Just elseBlock ->
              case typeBlock curScope elseBlock of
                Right elseAnnot ->
                  Right (IfStmt {cond, ifBody, elseBody = Just elseAnnot}, curScope)
                Left err -> Left err
            Nothing ->
              Right (IfStmt {cond, ifBody, elseBody = Nothing}, curScope)
        else Left $ "Condition in if statement must be of type BoolTy, got " ++ show condty
  | WhileStmt {cond, body} <- stmt = do
      cond <- typeExp curScope cond
      let condTy = cond.annot
      if condTy == BoolTy
        then do
          body <- typeBlock curScope body
          Right (WhileStmt {cond, body}, curScope)
        else Left $ "Condition in while statement must be of type BoolTy, got " ++ show condTy

typeBlock :: Scope.Scope -> RawBlock -> Either String TypedBlock
typeBlock scope Block {stmts} = do
  let innerScope = Scope.openScope "block" scope

  annotatedStmts <-
    foldM
      ( \(acc, nextScope) stmt -> do
          (annotatedStmt, nextScope) <- typeStmt nextScope stmt
          return (annotatedStmt : acc, nextScope)
      )
      ([], innerScope)
      stmts

  Right Block {annot = snd annotatedStmts, stmts = reverse . fst $ annotatedStmts}

typeFun :: Scope.Scope -> RawFun -> Either String TypedFun
typeFun scope Fun {id, args, retty, body = Block {stmts}} = do
  let innerScope = foldl (flip Scope.insertArg) (Scope.openScope id scope) args

  annotatedStmts <-
    foldM
      ( \(acc, nextScope) stmt -> do
          (annotatedStmt, nextScope) <- typeStmt nextScope stmt
          return (annotatedStmt : acc, nextScope)
      )
      ([], innerScope)
      stmts

  Right
    Fun
      { id,
        args,
        retty,
        body = Block {annot = snd annotatedStmts, stmts = reverse . fst $ annotatedStmts}
      }

typeProgram' :: Scope.Scope -> RawProgram -> Either String TypedProgram
typeProgram' scope Program {funcs} = do
  funcs <- mapM (typeFun scope) funcs
  case Scope.lookup "main" scope of
    Just Scope.Symbol {ty = FunTy {retty}} ->
      if retty == VoidTy
        then Right Program {funcs}
        else Left "Main function must return type Void"
    Just _ -> Left "main cannot be a variable"
    Nothing -> Right Program {funcs}

buildGlobalScope :: RawProgram -> Scope.Scope
buildGlobalScope Program {funcs} =
  foldl
    (flip Scope.insertFunction)
    Scope.Scope
      { name = "global",
        symbols = Map.empty,
        parent = Nothing
      }
    funcs

typeProgram :: RawProgram -> Either String TypedProgram
typeProgram program = typeProgram' (buildGlobalScope program) program

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module TypeCheck (typeCheck) where

import Ast
import Control.Monad (foldM)
import Scope qualified

type TypedExpr = Expr Ty

type TypedStmt = Stmt Ty Scope.Scope

type TypedBlock = Block Ty Scope.Scope

type TypedFun = Fun Ty Scope.Scope

type TypedProgram = Program Ty Scope.Scope

typeCheckOp :: Operator -> Ty -> Ty -> Either String Ty
typeCheckOp op lty rty =
  case op of
    Add
      | lty == I32 && rty == I32 -> Right I32
    Add -> Left $ "Type mismatch in addition: " ++ show lty ++ " + " ++ show rty
    Subtract
      | lty == I32 && rty == I32 -> Right I32
    Subtract -> Left $ "Type mismatch in subtraction: " ++ show lty ++ " - " ++ show rty
    Multiply
      | lty == I32 && rty == I32 -> Right I32
    Multiply -> Left $ "Type mismatch in multiplication: " ++ show lty ++ " * " ++ show rty
    Equal
      | lty == rty -> Right Bool
    Equal -> Left $ "Type mismatch in equality check: " ++ show lty ++ "== " ++ show rty
    NotEqual
      | lty == rty -> Right Bool
    NotEqual -> Left $ "Type mismatch in inequality check: " ++ show lty ++ " != " ++ show rty
    LessThan
      | lty == I32 && rty == I32 -> Right Bool
    LessThan -> Left $ "Type mismatch in less than check: " ++ show lty ++ " < " ++ show rty
    GreaterThan
      | lty == I32 && rty == I32 -> Right Bool
    GreaterThan -> Left $ "Type mismatch in greater than check: " ++ show lty ++ " > " ++ show rty
    LessThanOrEqual
      | lty == I32 && rty == I32 -> Right Bool
    LessThanOrEqual -> Left $ "Type mismatch in less than or equal check: " ++ show lty ++ " <= " ++ show rty
    GreaterThanOrEqual
      | lty == I32 && rty == I32 -> Right Bool
    GreaterThanOrEqual -> Left $ "Type mismatch in greater than or equal check: " ++ show lty ++ " >= " ++ show rty
    And
      | lty == Bool && rty == Bool -> Right Bool
    And -> Left $ "Type mismatch in logical AND: " ++ show lty ++ " && " ++ show rty
    Or
      | lty == Bool && rty == Bool -> Right Bool
    Or -> Left $ "Type mismatch in logical OR: " ++ show lty ++ "|| " ++ show rty
    Not
      | lty == Bool -> Right Bool
    Not -> Left $ "Type mismatch in logical NOT: expected Bool, got " ++ show lty
    Xor
      | lty == Bool && rty == Bool -> Right Bool
    Xor -> Left $ "Type mismatch in logical XOR: " ++ show lty ++ " ^ " ++ show rty
    Modulo
      | lty == I32 && rty == I32 -> Right I32
    Modulo -> Left $ "Type mismatch in modulo operation: " ++ show lty ++ " % " ++ show rty
    _ -> Left $ "Unsupported operator: " ++ show op

typeCheckExpr :: Scope.Scope -> RawExpr -> Either String TypedExpr
typeCheckExpr curScope expr
  | IdentifierExpr {id} <- expr =
      case Scope.lookup id curScope of
        Just Scope.Symbol {ty = FunTy {}} ->
          Left $ "Cannot use function " ++ id ++ " as variable"
        Just Scope.Symbol {ty} ->
          Right
            IdentifierExpr
              { annot = ty,
                id
              }
        Nothing -> Left $ "Undefined variable: " ++ id
  | NumberExpr {num} <- expr =
      Right
        NumberExpr
          { annot = I32,
            num
          }
  | BinExpr {left, op, right} <- expr = do
      left <- typeCheckExpr curScope left
      right <- typeCheckExpr curScope right
      let lty = exprAnnot left
          rty = exprAnnot right
      case typeCheckOp op lty rty of
        Right ty -> Right BinExpr {annot = ty, left, op, right}
        Left err -> Left err
  | Call {id, args} <- expr =
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
              argAnnotated <- mapM (typeCheckExpr curScope) args
              if (exprAnnot <$> argAnnotated) == argtys
                then
                  Right
                    ( Call
                        { annot = retty,
                          id,
                          args = argAnnotated
                        }
                    )
                else
                  Left $
                    "Argument type mismatch for function "
                      ++ id
                      ++ ": expected "
                      ++ show argtys
                      ++ ", got "
                      ++ show argAnnotated
        Just Scope.Symbol {ty} ->
          Left $ "Cannot call variable of type: " ++ show ty ++ " as function: " ++ id
        Nothing -> Left $ "Undefined function: " ++ id

typeCheckStmt :: Scope.Scope -> RawStmt -> Either String (TypedStmt, Scope.Scope)
typeCheckStmt curScope stmt
  | LetStmt {vardef = v@VarDef {ty}, expr} <- stmt = do
      expr <- typeCheckExpr curScope expr
      let exprTy = exprAnnot expr
      if exprTy == ty
        then
          Right
            ( LetStmt
                { vardef = v,
                  expr
                },
              Scope.insertVar v curScope
            )
        else Left $ "Type mismatch: expected " ++ show ty ++ ", got " ++ show exprTy
  | AssignStmt {id, expr} <- stmt = do
      case Scope.lookup id curScope of
        Just Scope.Symbol {ty = FunTy {}} -> Left $ "Cannot assign to function: " ++ id
        Just Scope.Symbol {ty} -> do
          expr <- typeCheckExpr curScope expr
          let exprty = exprAnnot expr
          if exprty == ty
            then
              Right
                ( AssignStmt
                    { id,
                      expr
                    },
                  curScope
                )
            else Left $ "Type mismatch in assignment: expected " ++ show ty ++ ", got " ++ show exprty
        Nothing -> Left $ "Undefined variable: " ++ id
  | ExprStmt {expr} <- stmt = do
      expr <- typeCheckExpr curScope expr
      Right (ExprStmt {expr}, curScope)
  | ReturnStmt {expr} <- stmt = do
      case Scope.lookup curScope.name curScope of
        Just Scope.Symbol {ty = FunTy {retty}} -> do
          expr <- typeCheckExpr curScope expr
          let expty = exprAnnot expr
          if retty == expty
            then
              Right (ReturnStmt {expr}, curScope)
            else
              Left $ "Return expression type: " ++ show expty ++ "doesn't match function return type: " ++ show retty
        Just _ -> Left "Cannot return from a variable"
        Nothing -> Left "unreachable: undefined function"
  | IfStmt {cond, ifBody, elseBody} <- stmt = do
      cond <- typeCheckExpr curScope cond
      let condty = exprAnnot cond
      if condty == Bool
        then do
          ifBody <- typeCheckBlock curScope ifBody
          case elseBody of
            Just elseBlock ->
              case typeCheckBlock curScope elseBlock of
                Right elseAnnot ->
                  Right
                    ( IfStmt
                        { cond,
                          ifBody,
                          elseBody = Just elseAnnot
                        },
                      curScope
                    )
                Left err -> Left err
            Nothing ->
              Right
                ( IfStmt
                    { cond,
                      ifBody,
                      elseBody = Nothing
                    },
                  curScope
                )
        else Left $ "Condition in if statement must be of type Bool, got " ++ show condty
  | WhileStmt {cond, body} <- stmt = do
      cond <- typeCheckExpr curScope cond
      let condTy = exprAnnot cond
      if condTy == Bool
        then do
          body <- typeCheckBlock curScope body
          Right
            ( WhileStmt
                { cond,
                  body
                },
              curScope
            )
        else Left $ "Condition in while statement must be of type Bool, got " ++ show condTy

typeCheckBlock :: Scope.Scope -> RawBlock -> Either String TypedBlock
typeCheckBlock curScope Block {stmts} = do
  let innerScope = Scope.openScope "block" curScope

  annotatedStmts <-
    foldM
      ( \(acc, nextScope) stmt -> do
          (annotatedStmt, nextScope) <- typeCheckStmt nextScope stmt
          return (annotatedStmt : acc, nextScope)
      )
      ([], innerScope)
      stmts

  Right
    ( Block
        { annot = snd annotatedStmts,
          stmts = reverse . fst $ annotatedStmts
        }
    )

typeCheckFunction :: Scope.Scope -> RawFun -> Either String TypedFun
typeCheckFunction curScope Fun {id, args, retty, body = Block {stmts}} = do
  let innerScope = foldl (flip Scope.insertVar) (Scope.openScope id curScope) args

  annotatedStmts <-
    foldM
      ( \(acc, nextScope) stmt -> do
          (annotatedStmt, nextScope) <- typeCheckStmt nextScope stmt
          return (annotatedStmt : acc, nextScope)
      )
      ([], innerScope)
      stmts

  Right
    ( Fun
        { id,
          args,
          retty,
          body =
            Block
              { annot = snd annotatedStmts,
                stmts = reverse . fst $ annotatedStmts
              }
        }
    )

typeCheckProgram :: Scope.Scope -> RawProgram -> Either String TypedProgram
typeCheckProgram scope Program {funcs} = do
  funcs <- mapM (typeCheckFunction scope) funcs
  case Scope.lookup "main" scope of
    -- Main function must return i32
    Just Scope.Symbol {ty = FunTy {retty}} ->
      if retty == Void
        then Right Program {funcs}
        else Left "Main function must return type Void"
    Just _ -> Left "main cannot be a variable"
    Nothing -> Right Program {funcs}

buildGlobalScope :: RawProgram -> Scope.Scope
buildGlobalScope Program {funcs} =
  foldl (flip Scope.insertFunction) Scope.newGlobalScope funcs

typeCheck :: RawProgram -> Either String TypedProgram
typeCheck program = typeCheckProgram (buildGlobalScope program) program

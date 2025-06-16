{-# LANGUAGE OverloadedRecordDot #-}

module TypeCheck (typeCheck) where

import Ast
import Control.Monad (foldM)
import Scope qualified

data TypeAnnotations = Annotations
  { currentScope :: Scope.Scope,
    nodeType :: Ty
  }
  deriving (Eq)

instance Show TypeAnnotations where
  show (Annotations {currentScope, nodeType}) =
    "{scope = " ++ show currentScope ++ ", ty = " ++ show nodeType ++ "}"

type TypedExpr = Expr TypeAnnotations

type TypedStmt = Stmt TypeAnnotations

type TypedBlock = Block TypeAnnotations

type TypedFun = Fun TypeAnnotations

type TypedProgram = Program TypeAnnotations

typeCheckExpr :: Scope.Scope -> RawExpr -> Either String TypedExpr
typeCheckExpr currentScope expr
  | IdentifierExpr _ name <- expr =
      case Scope.lookup name currentScope of
        Just (Scope.Variable ty) -> Right (IdentifierExpr (Annotations {currentScope, nodeType = ty}) name)
        Just (Scope.Function _ _) -> Left $ "Cannot use function " ++ name ++ " as variable"
        Nothing -> Left $ "Undefined variable: " ++ name
  | NumberExpr _ n <- expr =
      Right (NumberExpr (Annotations {currentScope, nodeType = I32}) n)
  | BinExpr _ left op right <- expr = do
      leftNode <- typeCheckExpr currentScope left
      rightNode <- typeCheckExpr currentScope right
      let leftTy = (getExprAnnotations leftNode).nodeType
          rightTy = (getExprAnnotations rightNode).nodeType
      case op of
        Add
          | leftTy == I32 && rightTy == I32 ->
              Right (Annotations {currentScope, nodeType = I32} <$ expr)
        Add ->
          Left $ "Type mismatch in addition: " ++ show leftTy ++ " + " ++ show rightTy
        Subtract
          | leftTy == I32 && rightTy == I32 ->
              Right (Annotations {currentScope, nodeType = I32} <$ expr)
        Subtract ->
          Left $ "Type mismatch in subtraction: " ++ show leftTy ++ " - " ++ show rightTy
        Multiply
          | leftTy == I32 && rightTy == I32 ->
              Right (Annotations {currentScope, nodeType = I32} <$ expr)
        Multiply ->
          Left $ "Type mismatch in multiplication: " ++ show leftTy ++ " * " ++ show rightTy
        Equal
          | leftTy == rightTy ->
              Right (Annotations {currentScope, nodeType = Bool} <$ expr)
        Equal ->
          Left $ "Type mismatch in equality check: " ++ show leftTy ++ " == " ++ show rightTy
        NotEqual
          | leftTy == rightTy ->
              Right (Annotations {currentScope, nodeType = Bool} <$ expr)
        NotEqual ->
          Left $ "Type mismatch in inequality check: " ++ show leftTy ++ " != " ++ show rightTy
        LessThan
          | leftTy == I32 && rightTy == I32 ->
              Right (Annotations {currentScope, nodeType = Bool} <$ expr)
        LessThan ->
          Left $ "Type mismatch in less than check: " ++ show leftTy ++ " < " ++ show rightTy
        GreaterThan
          | leftTy == I32 && rightTy == I32 ->
              Right (Annotations {currentScope, nodeType = Bool} <$ expr)
        GreaterThan ->
          Left $ "Type mismatch in greater than check: " ++ show leftTy ++ " > " ++ show rightTy
        LessThanOrEqual
          | leftTy == I32 && rightTy == I32 ->
              Right (Annotations {currentScope, nodeType = Bool} <$ expr)
        LessThanOrEqual ->
          Left $ "Type mismatch in less than or equal check: " ++ show leftTy ++ " <= " ++ show rightTy
        GreaterThanOrEqual
          | leftTy == I32 && rightTy == I32 ->
              Right (Annotations {currentScope, nodeType = Bool} <$ expr)
        GreaterThanOrEqual ->
          Left $ "Type mismatch in greater than or equal check: " ++ show leftTy ++ " >= " ++ show rightTy
        And
          | leftTy == Bool && rightTy == Bool ->
              Right (Annotations {currentScope, nodeType = Bool} <$ expr)
        And ->
          Left $ "Type mismatch in logical AND: " ++ show leftTy ++ " && " ++ show rightTy
        Or
          | leftTy == Bool && rightTy == Bool ->
              Right (Annotations {currentScope, nodeType = Bool} <$ expr)
        Or ->
          Left $ "Type mismatch in logical OR: " ++ show leftTy ++ " || " ++ show rightTy
        Not
          | leftTy == Bool ->
              Right (Annotations {currentScope, nodeType = Bool} <$ expr)
        Not ->
          Left $ "Type mismatch in logical NOT: expected Bool, got " ++ show leftTy
        Xor
          | leftTy == Bool && rightTy == Bool ->
              Right (Annotations {currentScope, nodeType = Bool} <$ expr)
        Xor ->
          Left $ "Type mismatch in logical XOR: " ++ show leftTy ++ " ^ " ++ show rightTy
        Modulo
          | leftTy == I32 && rightTy == I32 ->
              Right (Annotations {currentScope, nodeType = I32} <$ expr)
        Modulo ->
          Left $ "Type mismatch in modulo operation: " ++ show leftTy ++ " % " ++ show rightTy
        _ -> Left $ "Unsupported operator: " ++ show op
  | Call _ callFunName callArgs <- expr =
      case Scope.lookup callFunName currentScope of
        Just (Scope.Function funArgTys funRetTy) ->
          if length callArgs /= length funArgTys
            then
              Left $
                "Function "
                  ++ callFunName
                  ++ " expects "
                  ++ show (length funArgTys)
                  ++ " arguments, got "
                  ++ show (length callArgs)
            else do
              argAnnotated <- mapM (typeCheckExpr currentScope) callArgs
              let argTypes = nodeType . getExprAnnotations <$> argAnnotated
              if argTypes == funArgTys
                then
                  Right
                    ( Call
                        { callAnnot =
                            ( Annotations {currentScope, nodeType = funRetTy}
                            ),
                          funCallName = callFunName,
                          funCallArgs = argAnnotated
                        }
                    )
                else
                  Left $
                    "Argument type mismatch for function "
                      ++ callFunName
                      ++ ": expected "
                      ++ show funArgTys
                      ++ ", got "
                      ++ show argAnnotated
        Just (Scope.Variable _) ->
          Left $ "Cannot call variable as function: " ++ callFunName
        Nothing -> Left $ "Undefined function: " ++ callFunName

typeCheckStmt :: Scope.Scope -> RawStmt -> Either String TypedStmt
typeCheckStmt currentScope stmt
  | LetStmt _ v@(VarDef _ ty) expr <- stmt = do
      exprAnnot <- typeCheckExpr currentScope expr
      let exprTy = nodeType $ getExprAnnotations exprAnnot
      if exprTy == ty
        then
          Right
            ( LetStmt
                { letAnnot =
                    Annotations
                      { currentScope = Scope.insertVar v currentScope,
                        nodeType = Void
                      },
                  letVarDef = v,
                  letVarExpr = exprAnnot
                }
            )
        else Left $ "Type mismatch: expected " ++ show ty ++ ", got " ++ show exprTy
  | AssignStmt _ name expr <- stmt = do
      case Scope.lookup name currentScope of
        Just (Scope.Variable expectedTy) -> do
          exprAnnot <- typeCheckExpr currentScope expr
          let exprTy = nodeType $ getExprAnnotations exprAnnot
          if exprTy == expectedTy
            then Right (Annotations {currentScope, nodeType = Void} <$ stmt)
            else Left $ "Type mismatch in assignment: expected " ++ show expectedTy ++ ", got " ++ show exprTy
        Just (Scope.Function _ _) -> Left $ "Cannot assign to function: " ++ name
        Nothing -> Left $ "Undefined variable: " ++ name
  | ExprStmt expr <- stmt = do
      nodeAnnot <- typeCheckExpr currentScope expr
      let expType = nodeType $ getExprAnnotations nodeAnnot
      Right (Annotations {currentScope, nodeType = expType} <$ stmt)
  | ReturnStmt _ expr <- stmt = do
      exprAnnot <- typeCheckExpr currentScope expr
      let expTy = (getExprAnnotations exprAnnot).nodeType
      case Scope.lookup currentScope.scopeName currentScope of
        Just (Scope.Function _ retTy) ->
          if retTy == expTy
            then
              Right $ Annotations {currentScope, nodeType = retTy} <$ stmt
            else
              Left $ "Return expression type: " ++ show expTy ++ "doesn't match function return type: " ++ show retTy
        Just (Scope.Variable _) -> Left "Cannot return from a variable"
        Nothing -> Left "unreachable: undefined function"
  | IfStmt _ cond body elseBody <- stmt = do
      ifCond <- typeCheckExpr currentScope cond
      let condTy = (getExprAnnotations ifCond).nodeType
      if condTy == Bool
        then do
          ifBody <- typeCheckBlock currentScope body
          case elseBody of
            Just elseBlock ->
              case typeCheckBlock currentScope elseBlock of
                Right elseAnnot ->
                  Right
                    ( IfStmt
                        { ifAnnot = Annotations {currentScope, nodeType = Void},
                          ifCond,
                          ifBody,
                          elseBody = Just elseAnnot
                        }
                    )
                Left err -> Left err
            Nothing ->
              Right
                ( IfStmt
                    { ifAnnot = Annotations {currentScope, nodeType = Void},
                      ifCond,
                      ifBody,
                      elseBody = Nothing
                    }
                )
        else Left $ "Condition in if statement must be of type Bool, got " ++ show condTy
  | WhileStmt _ cond body <- stmt = do
      whileCond <- typeCheckExpr currentScope cond
      let condTy = (getExprAnnotations whileCond).nodeType
      if condTy == Bool
        then do
          whileBody <- typeCheckBlock currentScope body
          Right
            ( WhileStmt
                { whileAnnot = Annotations {currentScope, nodeType = Void},
                  whileCond,
                  whileBody
                }
            )
        else Left $ "Condition in while statement must be of type Bool, got " ++ show condTy

typeCheckBlock :: Scope.Scope -> RawBlock -> Either String TypedBlock
typeCheckBlock table (Block _ stmts) = do
  -- create a new scope for the block
  let newTable = Scope.openScope "block" table
  -- type check each statement in the block
  annotatedStmts <-
    foldM
      ( \(acc, nextTable) stmt -> do
          annotatedStmt <- typeCheckStmt nextTable stmt
          nextTable' <- case getStmtAnnotations annotatedStmt of
            Annotations {currentScope} -> Right currentScope
          return (annotatedStmt : acc, nextTable')
      )
      ([], newTable)
      stmts
  -- return the annotated block
  Right (Block {blockAnnot = Annotations {currentScope = snd annotatedStmts, nodeType = Void}, blockStmts = (reverse . fst $ annotatedStmts)})

typeCheckFunction :: Scope.Scope -> RawFun -> Either String TypedFun
typeCheckFunction currentScope (Fun name args retTy (Block _ stmts)) = do
  -- push the new scope for the function body
  let bodyScope =
        foldl (flip Scope.insertVar) (Scope.openScope name currentScope) args

  -- type check the function body
  annotatedStmts <-
    foldM
      ( \(acc, nextScope) stmt -> do
          annotatedStmt <- typeCheckStmt nextScope stmt
          nextScope' <- case getStmtAnnotations annotatedStmt of
            Annotations {currentScope} -> Right currentScope
          return (annotatedStmt : acc, nextScope')
      )
      ([], bodyScope)
      stmts
  -- return the annotated function
  Right
    ( Fun
        { funName = name,
          funArgs = args,
          funRetTy = retTy,
          funBody = Block {blockAnnot = Annotations {currentScope = snd annotatedStmts, nodeType = Void}, blockStmts = (reverse . fst $ annotatedStmts)}
        }
    )

typeCheckProgram :: Scope.Scope -> RawProgram -> Either String TypedProgram
typeCheckProgram table (Program funcs) = do
  funcTable <- mapM (typeCheckFunction table) funcs
  case Scope.lookup "main" table of
    -- Main function must return i32
    Just (Scope.Function _ retTy) ->
      if retTy == Void
        then Right (Program funcTable)
        else Left "Main function must return type Void"
    Just (Scope.Variable _) -> Left "main cannot be a variable"
    Nothing -> Right (Program funcTable)

buildFunctionTable :: RawProgram -> Scope.Scope
buildFunctionTable (Program funcs) =
  foldl (flip Scope.insertFunction) Scope.newGlobalScope funcs

typeCheck :: RawProgram -> Either String TypedProgram
typeCheck program = do
  let initialTable = buildFunctionTable program
  typeCheckProgram initialTable program

module TypeCheck (typeCheck) where

import Ast
import Control.Monad (foldM)
import SymbolTable qualified as ST

data Annotations = Annotations
  { currentScope :: AnnotTable,
    nodeType :: Ty
  }
  deriving (Show, Eq)

type AnnotTable = ST.SymbolTable Annotations

type AnnotExpr = Expr Annotations

type AnnotStmt = Stmt Annotations

type AnnotBlock = Block Annotations

type AnnotFun = Fun Annotations

type AnnotProgram = Program Annotations

typeCheckExpr :: AnnotTable -> RawExpr -> Either String AnnotExpr
typeCheckExpr currentScope ident@(IdentifierExpr _ name) =
  case ST.lookup name currentScope of
    Just (ST.Variable ty) -> Right (Annotations {currentScope, nodeType = ty} <$ ident)
    Just (ST.Function _ _) -> Left $ "Cannot use function " ++ name ++ " as variable"
    Nothing -> Left $ "Undefined variable: " ++ name
typeCheckExpr currentScope num@(NumberExpr _ _) =
  Right (Annotations {currentScope, nodeType = I32} <$ num)
typeCheckExpr currentScope bine@(BinExpr _ left op right) = do
  leftNode <- typeCheckExpr currentScope left
  rightNode <- typeCheckExpr currentScope right
  let leftTy = nodeType $ getExprAnnotations leftNode
      rightTy = nodeType $ getExprAnnotations rightNode
  case op of
    Add ->
      if leftTy == I32 && rightTy == I32
        then Right (Annotations {currentScope, nodeType = I32} <$ bine)
        else Left $ "Type mismatch in addition: " ++ show leftTy ++ " + " ++ show rightTy
    Subtract ->
      if leftTy == I32 && rightTy == I32
        then Right (Annotations {currentScope, nodeType = I32} <$ bine)
        else Left $ "Type mismatch in subtraction: " ++ show leftTy ++ " - " ++ show rightTy
    Multiply ->
      if leftTy == I32 && rightTy == I32
        then Right (Annotations {currentScope, nodeType = I32} <$ bine)
        else Left $ "Type mismatch in multiplication: " ++ show leftTy ++ " * " ++ show rightTy
    Equal ->
      if leftTy == rightTy
        then Right (Annotations {currentScope, nodeType = Bool} <$ bine)
        else Left $ "Type mismatch in equality check: " ++ show leftTy ++ " == " ++ show rightTy
    NotEqual ->
      if leftTy == rightTy
        then Right (Annotations {currentScope, nodeType = Bool} <$ bine)
        else Left $ "Type mismatch in inequality check: " ++ show leftTy ++ " != " ++ show rightTy
    LessThan ->
      if leftTy == I32 && rightTy == I32
        then Right (Annotations {currentScope, nodeType = Bool} <$ bine)
        else Left $ "Type mismatch in less than check: " ++ show leftTy ++ " < " ++ show rightTy
    GreaterThan ->
      if leftTy == I32 && rightTy == I32
        then
          Right
            (Annotations {currentScope, nodeType = Bool} <$ bine)
        else Left $ "Type mismatch in greater than check: " ++ show leftTy ++ " > " ++ show rightTy
    LessThanOrEqual ->
      if leftTy == I32 && rightTy == I32
        then Right (Annotations {currentScope, nodeType = Bool} <$ bine)
        else Left $ "Type mismatch in less than or equal check: " ++ show leftTy ++ " <= " ++ show rightTy
    GreaterThanOrEqual ->
      if leftTy == I32 && rightTy == I32
        then Right (Annotations {currentScope, nodeType = Bool} <$ bine)
        else Left $ "Type mismatch in greater than or equal check: " ++ show leftTy ++ " >= " ++ show rightTy
    And ->
      if leftTy == Bool && rightTy == Bool
        then Right (Annotations {currentScope, nodeType = Bool} <$ bine)
        else Left $ "Type mismatch in logical AND: " ++ show leftTy ++ " && " ++ show rightTy
    Or ->
      if leftTy == Bool && rightTy == Bool
        then Right (Annotations {currentScope, nodeType = Bool} <$ bine)
        else Left $ "Type mismatch in logical OR: " ++ show leftTy ++ " || " ++ show rightTy
    Not ->
      if leftTy == Bool
        then Right (Annotations {currentScope, nodeType = Bool} <$ bine)
        else Left $ "Type mismatch in logical NOT: expected Bool, got " ++ show leftTy
    Xor ->
      if leftTy == Bool && rightTy == Bool
        then Right (Annotations {currentScope, nodeType = Bool} <$ bine)
        else Left $ "Type mismatch in logical XOR: " ++ show leftTy ++ " ^ " ++ show rightTy
    Modulo ->
      if leftTy == I32 && rightTy == I32
        then Right (Annotations {currentScope, nodeType = I32} <$ bine)
        else Left $ "Type mismatch in modulo operation: " ++ show leftTy ++ " % " ++ show rightTy
    _ -> Left $ "Unsupported operator: " ++ show op
typeCheckExpr currentScope call@(Call _ callFunName callArgs) = do
  case ST.lookupFunction callFunName currentScope of
    Just (Ast.Fun funName funArgs funRetTy _) -> do
      if length callArgs /= length funArgs
        then
          Left $
            "Function "
              ++ funName
              ++ " expects "
              ++ show (length funArgs)
              ++ " arguments, got "
              ++ show (length callArgs)
        else do
          argAnnotated <- mapM (typeCheckExpr currentScope) callArgs
          let argTypes = nodeType . getExprAnnotations <$> argAnnotated
              expectedArgTypes = (\(VarDef _ ty) -> ty) <$> funArgs
          if argTypes == expectedArgTypes
            then Right (Annotations {currentScope, nodeType = funRetTy} <$ call)
            else
              Left $
                "Argument type mismatch for function "
                  ++ funName
                  ++ ": expected "
                  ++ show expectedArgTypes
                  ++ ", got "
                  ++ show argAnnotated
    Nothing -> Left $ "Undefined function: " ++ callFunName

typeCheckStmt :: AnnotTable -> RawStmt -> Either String AnnotStmt
typeCheckStmt table (LetStmt varDef@(VarDef _ ty) expr) = do
  exprTy <- typeCheckExpr table expr
  if exprTy == ty
    then Right $ ST.insertVar varDef table
    else Left $ "Type mismatch: expected " ++ show ty ++ ", got " ++ show exprTy
typeCheckStmt table (AssignStmt name expr) = do
  case ST.lookup name table of
    Just (ST.Variable expectedTy) -> do
      exprTy <- typeCheckExpr table expr
      if exprTy == expectedTy
        then Right table
        else Left $ "Type mismatch in assignment: expected " ++ show expectedTy ++ ", got " ++ show exprTy
    Just (ST.Function _ _) -> Left $ "Cannot assign to function: " ++ name
    Nothing -> Left $ "Undefined variable: " ++ name
typeCheckStmt table (ExprStmt expr) = do
  _ <- typeCheckExpr table expr
  Right table
typeCheckStmt table (ReturnStmt expr) = do
  exprTy <- typeCheckExpr table expr
  retTy <- case ST.lookupFunction "main" table of
    Just (Fun _ _ retTy _) -> Right retTy
    Nothing -> Left "No return type found for main function"
  if exprTy == retTy
    then Right table
    else Left $ "Return type mismatch: expected " ++ show retTy ++ ", got " ++ show exprTy
typeCheckStmt table (IfStmt cond body elseBody) = do
  condTy <- typeCheckExpr table cond
  if condTy == Bool
    then do
      bodyTable <- typeCheckBlock table body
      case elseBody of
        Just elseBlock -> typeCheckBlock bodyTable elseBlock
        Nothing -> Right bodyTable
    else Left $ "Condition in if statement must be of type Bool, got " ++ show condTy
typeCheckStmt table (WhileStmt cond body) = do
  condTy <- typeCheckExpr table cond
  if condTy == Bool
    then typeCheckBlock table body
    else Left $ "Condition in while statement must be of type Bool, got " ++ show condTy

typeCheckBlock :: AnnotTable -> RawBlock -> Either String AnnotBlock
typeCheckBlock table (Block stmts) = do
  -- create a new scope for the block
  let newTable = ST.pushScope table
  foldM typeCheckStmt newTable stmts >>= \finalTable ->
    -- pop the scope after processing all statements
    Right (ST.popScope finalTable)

typeCheckFunction :: AnnotTable -> RawFun -> Either String AnnotFun
typeCheckFunction table (Fun _ args _ body) = do
  -- create new scope and add function arguments
  let argTable = foldl (\t (VarDef argName argTy) -> ST.insertVar (VarDef argName argTy) t) table args
  -- push the new scope for the function body
  let bodyTable = ST.pushScope argTable
  -- type check the function body
  ST.popScope <$> typeCheckBlock bodyTable body

typeCheckProgram :: AnnotTable -> RawProgram -> Either String AnnotProgram
typeCheckProgram table (Program funcs) = do
  funcTable <- foldM typeCheckFunction table funcs
  case ST.lookupFunction "main" funcTable of
    -- Main function must return i32
    Just (Fun _ _ retTy _) ->
      if retTy == Void
        then Right funcTable
        else Left "Main function must return type Void"
    Nothing -> Right funcTable

buildFunctionTable :: RawProgram -> AnnotTable
buildFunctionTable (Program funcs) = foldl (flip ST.insertFunction) ST.empty funcs

typeCheck :: RawProgram -> Either String AnnotProgram
typeCheck program = do
  let initialTable = buildFunctionTable program
  typeCheckProgram initialTable program

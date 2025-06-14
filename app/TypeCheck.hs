module TypeCheck (typeCheck) where

import Ast
import Control.Monad (foldM)
import SymbolTable qualified as ST

typeCheckExpr :: ST.SymbolTable -> Expr -> Either String Ty
typeCheckExpr table (IdentifierExpr name) =
  case ST.lookup name table of
    Just (ST.Variable ty) -> Right ty
    Just (ST.Function _ _) -> Left $ "Cannot use function " ++ name ++ " as variable"
    Nothing -> Left $ "Undefined variable: " ++ name
typeCheckExpr _ (NumberExpr _) = Right I32
typeCheckExpr table (BinExpr left op right) = do
  leftTy <- typeCheckExpr table left
  rightTy <- typeCheckExpr table right
  case op of
    Add -> if leftTy == I32 && rightTy == I32 then Right I32 else Left "Addition requires both operands to be i32"
    Subtract -> if leftTy == I32 && rightTy == I32 then Right I32 else Left "Subtraction requires both operands to be i32"
    Multiply -> if leftTy == I32 && rightTy == I32 then Right I32 else Left "Multiplication requires both operands to be i32"
    Assign -> if leftTy == I32 && rightTy == I32 then Right I32 else Left "Assignment requires both operands to be i32"
    Equal -> if leftTy == rightTy then Right Bool else Left $ "Equality check requires both operands to be of the same type: " ++ show leftTy ++ " and " ++ show rightTy
    LessThan -> if leftTy == I32 && rightTy == I32 then Right Bool else Left "Less than operator requires both operands to be i32"
    GreaterThan -> if leftTy == I32 && rightTy == I32 then Right Bool else Left "Greater than operator requires both operands to be i32"
    LessThanOrEqual -> if leftTy == I32 && rightTy == I32 then Right Bool else Left "Less than or equal operator requires both operands to be i32"
    GreaterThanOrEqual -> if leftTy == I32 && rightTy == I32 then Right Bool else Left "Greater than or equal operator requires both operands to be i32"
    NotEqual -> if leftTy == rightTy then Right Bool else Left $ "Not equal check requires both operands to be of the same type: " ++ show leftTy ++ " and " ++ show rightTy
    And -> if leftTy == Bool && rightTy == Bool then Right Bool else Left "And operator requires both operands to be bool"
    Or -> if leftTy == Bool && rightTy == Bool then Right Bool else Left "Or operator requires both operands to be bool"
    Not -> if leftTy == Bool then Right Bool else Left $ "Not operator requires operand to be bool but got " ++ show leftTy
    Xor -> if leftTy == Bool && rightTy == Bool then Right Bool else Left "Xor operator requires both operands to be bool"
    Modulo -> if leftTy == I32 && rightTy == I32 then Right I32 else Left "Modulo operator requires both operands to be i32"
typeCheckExpr table (Call callFunName callArgs) = do
  case ST.lookupFunction callFunName table of
    Just (Ast.Fun funName funArgs funRetTy _) -> do
      if length callArgs /= length funArgs
        then Left $ "Function " ++ funName ++ " expects " ++ show (length funArgs) ++ " arguments, got " ++ show (length callArgs)
        else do
          argTypes <- mapM (typeCheckExpr table) callArgs
          let expectedArgTypes = map (\(VarDef _ ty) -> ty) funArgs
          if argTypes == expectedArgTypes
            then Right funRetTy
            else Left $ "Argument type mismatch for function " ++ funName ++ ": expected " ++ show expectedArgTypes ++ ", got " ++ show argTypes
    Nothing -> Left $ "Undefined function: " ++ callFunName

typeCheckStmt :: ST.SymbolTable -> Stmt -> Either String ST.SymbolTable
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

typeCheckBlock :: ST.SymbolTable -> Block -> Either String ST.SymbolTable
typeCheckBlock table (Block stmts) = do
  -- create a new scope for the block
  let newTable = ST.pushScope table
  foldM typeCheckStmt newTable stmts >>= \finalTable ->
    -- pop the scope after processing all statements
    Right (ST.popScope finalTable)

typeCheckFunction :: ST.SymbolTable -> Fun -> Either String ST.SymbolTable
typeCheckFunction table (Fun _ args _ body) = do
  -- create new scope and add function arguments
  let argTable = foldl (\t (VarDef argName argTy) -> ST.insertVar (VarDef argName argTy) t) table args
  -- push the new scope for the function body
  let bodyTable = ST.pushScope argTable
  -- type check the function body
  ST.popScope <$> typeCheckBlock bodyTable body

typeCheckProgram :: ST.SymbolTable -> Program -> Either String ST.SymbolTable
typeCheckProgram table (Program funcs) = do
  funcTable <- foldM typeCheckFunction table funcs
  case ST.lookupFunction "main" funcTable of
    -- Main function must return i32
    Just (Fun _ _ retTy _) ->
      if retTy == I32
        then Right funcTable
        else Left "Main function must return type i32"
    Nothing -> Left "Main function not found"

buildFunctionTable :: Program -> ST.SymbolTable
buildFunctionTable (Program funcs) = foldl (flip ST.insertFunction) ST.empty funcs

typeCheck :: Program -> Either String ST.SymbolTable
typeCheck program = do
  let initialTable = buildFunctionTable program
  typeCheckProgram initialTable program

module Ast.Semant.Expressions
  ( typeBinOp,
    typeUnaryOp,
    typeExp,
    annotateIllegalExpAsVoid,
  )
where

import Ast.Lenses
import Ast.Semant.State
import Ast.Types
import Control.Lens
import Control.Monad (when)
import Control.Monad.State (State)
import SymbolTable.Types (Symbol (..))
import TypeSystem

-- | Type check a binary operator
typeBinOp :: BinOp -> TypedExp -> TypedExp -> State TypingState Ty
typeBinOp op' Exp {_expAnnot = lty} Exp {_expAnnot = rty}
  | (lty, rty) == (IntTy, IntTy) =
      case op' of
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
          addErrorInState ("Unsupported operator for IntTy: " ++ show op')
          return VoidTy
  | (lty, rty) == (BoolTy, BoolTy) =
      case op' of
        And -> return BoolTy
        Or -> return BoolTy
        Xor -> return BoolTy
        Equal -> return BoolTy
        NotEqual -> return BoolTy
        _ -> do
          addErrorInState ("Unsupported operator for BoolTy: " ++ show op')
          return VoidTy
  | (lty, rty) == (CharTy, CharTy) =
      case op' of
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
          addErrorInState ("Unsupported operator for CharTy: " ++ show op')
          return VoidTy
  | otherwise = do
      addErrorInState ("Type mismatch in operator: " ++ show lty ++ " " ++ show op' ++ " " ++ show rty)
      return VoidTy

-- | Type check a unary operator
typeUnaryOp :: UnaryOp -> TypedExp -> State TypingState Ty
typeUnaryOp op' Exp {_expAnnot}
  | IntTy <- _expAnnot =
      case op' of
        UnarySub -> return IntTy
        UnaryNot -> return BoolTy
        _ -> do
          addErrorInState ("Unsupported unary operator for IntTy: " ++ show op')
          return VoidTy
  | BoolTy <- _expAnnot =
      case op' of
        UnaryNot -> return BoolTy
        _ -> do
          addErrorInState ("Unsupported unary operator for BoolTy: " ++ show op')
          return VoidTy
  | CharTy <- _expAnnot =
      case op' of
        UnarySub -> return CharTy
        UnaryNot -> return BoolTy
        _ -> do
          addErrorInState ("Unsupported unary operator for CharTy: " ++ show op')
          return VoidTy
  | PtrTy {ptrTyElemTy} <- _expAnnot =
      case op' of
        UnaryPtrDeref -> return ptrTyElemTy
        _ -> do
          addErrorInState ("Can only dereference a pointer with '*', got: " ++ show _expAnnot)
          return VoidTy
  | otherwise = do
      addErrorInState ("Type mismatch in unary operator: " ++ show _expAnnot ++ " " ++ show op')
      return VoidTy

-- | Annotate an illegal expression with VoidTy
annotateIllegalExpAsVoid :: ExpInner Ty -> TypedExp
annotateIllegalExpAsVoid expInnerTyped =
  Exp
    { _expAnnot = VoidTy,
      _expInner = expInnerTyped
    }

-- | Type check an expression
typeExp :: RawExp -> State TypingState TypedExp
typeExp expression = case expression ^. expInner of
  IdExp {idName} -> do
    symb <- lookupSymbolInState idName
    case symb of
      Just VarSymbol {_varSymbolTy, _varSymbolStorage} -> do
        return
          Exp
            { _expAnnot = _varSymbolTy,
              _expInner = IdExp {idName}
            }
      Just ArgSymbol {_argSymbolTy, _argSymbolStorage} -> do
        return
          Exp
            { _expAnnot = _argSymbolTy,
              _expInner = IdExp {idName}
            }
      Just FunSymbol {_funSymbolTy} -> do
        addErrorInState ("Cannot use function " ++ idName ++ " as variable")
        return $ Exp {_expAnnot = _funSymbolTy, _expInner = IdExp {idName}}
      Nothing -> do
        addErrorInState ("Undefined variable: " ++ idName)
        return $ annotateIllegalExpAsVoid (IdExp {idName})
  NumberExp {numberValue} ->
    return
      Exp
        { _expAnnot = IntTy,
          _expInner = NumberExp {numberValue}
        }
  CharExp {charValue} ->
    return
      Exp
        { _expAnnot = CharTy,
          _expInner = CharExp {charValue}
        }
  BinExp {binLeft, binOp, binRight} -> do
    binLeft' <- typeExp binLeft
    binRight' <- typeExp binRight
    opTy <- typeBinOp binOp binLeft' binRight'
    return
      Exp
        { _expAnnot = opTy,
          _expInner = BinExp binLeft' binOp binRight'
        }
  UnaryExp {unaryOp, unaryExp} -> do
    unaryExp' <- typeExp unaryExp
    opTy <- typeUnaryOp unaryOp unaryExp'
    return Exp {_expAnnot = opTy, _expInner = UnaryExp unaryOp unaryExp'}
  CallExp {callId, callArgs} -> do
    symb <- lookupSymbolInState callId
    callArgs' <- mapM typeExp callArgs

    case symb of
      Just FunSymbol {_funSymbolTy = FunTy {funTyArgs, funTyRetTy}} -> do
        let argtys' = map (^. expAnnot) callArgs'
        when (argtys' /= funTyArgs) $
          addErrorInState
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

        return
          Exp
            { _expAnnot = funTyRetTy,
              _expInner = CallExp {callId, callArgs = callArgs'}
            }
      Just FunSymbol {_funSymbolTy} -> do
        addErrorInState ("Internal error: expected function type, got " ++ show _funSymbolTy)
        return $ annotateIllegalExpAsVoid (CallExp {callId, callArgs = callArgs'})
      Just VarSymbol {_varSymbolTy, _varSymbolStorage} -> do
        addErrorInState ("Cannot call variable: " ++ callId ++ " of type: " ++ show _varSymbolTy)
        return $ annotateIllegalExpAsVoid (CallExp {callId, callArgs = callArgs'})
      Just ArgSymbol {_argSymbolTy, _argSymbolStorage} -> do
        addErrorInState ("Cannot call argument: " ++ callId ++ " of type: " ++ show _argSymbolTy)
        return $ annotateIllegalExpAsVoid (CallExp {callId, callArgs = callArgs'})
      Nothing -> do
        addErrorInState ("Undefined function: " ++ callId)
        return $ annotateIllegalExpAsVoid (CallExp {callId, callArgs = callArgs'})
  ArrAccessExp {arrId, arrIndex} -> do
    symb <- lookupSymbolInState arrId
    arrIndex' <- typeExp arrIndex
    let arrIndexTy = arrIndex' ^. expAnnot

    case symb of
      Just VarSymbol {_varSymbolTy = ArrTy {arrTyElemTy}} -> do
        when (arrIndexTy /= IntTy) $
          addErrorInState ("Array index must be of type IntTy, got " ++ show arrIndexTy)
        return
          Exp
            { _expAnnot = arrTyElemTy,
              _expInner = ArrAccessExp {arrId, arrIndex = arrIndex'}
            }
      Just VarSymbol {_varSymbolTy} -> do
        addErrorInState ("Cannot access array element of variable: " ++ arrId ++ " of type: " ++ show _varSymbolTy)
        return $ annotateIllegalExpAsVoid (ArrAccessExp {arrId, arrIndex = arrIndex'})
      -- TODO: passing arrays as arguments is not supported yet
      Just ArgSymbol {} -> do
        when (arrIndexTy /= IntTy) $
          addErrorInState ("Passing arrays as arguments is not supported yet, got index of type " ++ show arrIndexTy)
        return
          Exp
            { _expAnnot = VoidTy,
              _expInner = ArrAccessExp {arrId, arrIndex = arrIndex'}
            }
      Just FunSymbol {_funSymbolTy} -> do
        addErrorInState ("Cannot access array element of function: " ++ arrId ++ " of type: " ++ show _funSymbolTy)
        return $ annotateIllegalExpAsVoid (ArrAccessExp {arrId, arrIndex = arrIndex'})
      Nothing -> do
        addErrorInState ("Undefined variable: " ++ arrId)
        return $ annotateIllegalExpAsVoid (ArrAccessExp {arrId, arrIndex = arrIndex'})
  TakeAddressExp {takeAddressId} -> do
    symb <- lookupSymbolInState takeAddressId
    case symb of
      Just VarSymbol {_varSymbolTy, _varSymbolStorage} -> do
        return
          Exp
            { _expAnnot = PtrTy {ptrTyElemTy = _varSymbolTy},
              _expInner = TakeAddressExp {takeAddressId}
            }
      Just ArgSymbol {_argSymbolTy, _argSymbolStorage} -> do
        return
          Exp
            { _expAnnot = PtrTy {ptrTyElemTy = _argSymbolTy},
              _expInner = TakeAddressExp {takeAddressId}
            }
      Just FunSymbol {_funSymbolTy} -> do
        addErrorInState ("Cannot take address of function: " ++ takeAddressId ++ " of type: " ++ show _funSymbolTy)
        return $ annotateIllegalExpAsVoid (TakeAddressExp {takeAddressId})
      Nothing -> do
        addErrorInState ("Undefined variable: " ++ takeAddressId)
        return $ annotateIllegalExpAsVoid (TakeAddressExp {takeAddressId})

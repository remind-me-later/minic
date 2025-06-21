{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module X86.Translate where

import Ast.Types qualified
import Control.Monad.State (State, get, modify, runState)
import Data.Map (Map, empty, fromList, lookup)
import Mir.Types qualified
import X86.Types qualified as X86

data TranslationState = TranslationState
  { stackFrameVars :: Map Ast.Types.Id Int,
    assemblyCode :: [X86.Inst],
    lastJmpCond :: Maybe X86.JmpCond,
    fileHeader :: String
  }

instance Show TranslationState where
  show TranslationState {assemblyCode, fileHeader} =
    fileHeader ++ "\n" ++ unlines (show <$> assemblyCode)

changeFlagOp :: X86.JmpCond -> State TranslationState ()
changeFlagOp cond = modify (\s -> s {lastJmpCond = Just cond})

flagOp :: State TranslationState (Maybe X86.JmpCond)
flagOp = do
  TranslationState {lastJmpCond} <- get
  return lastJmpCond

emitAsmInst :: X86.Inst -> State TranslationState ()
emitAsmInst code = do
  TranslationState {assemblyCode} <- get
  modify (\s -> s {assemblyCode = assemblyCode ++ [code]})

-- Offset from rbp (base pointer) for local variables
-- All variables are QWORDS and aligned
getVarOffset :: Ast.Types.Id -> State TranslationState (Maybe Int)
getVarOffset varId = do
  TranslationState {stackFrameVars} <- get
  return $ Data.Map.lookup varId stackFrameVars

loadVarToEax :: Ast.Types.Id -> State TranslationState ()
loadVarToEax varId = do
  maybeOffset <- getVarOffset varId
  case maybeOffset of
    Just offset ->
      emitAsmInst $
        X86.Mov
          { X86.src = X86.Mem X86.Rbp offset,
            X86.dst = X86.Reg X86.Rax
          }
    Nothing -> error $ "Variable " ++ show varId ++ " not found in stack frame"

storeEaxToVar :: Ast.Types.Id -> State TranslationState ()
storeEaxToVar varId = do
  maybeOffset <- getVarOffset varId
  case maybeOffset of
    Just offset ->
      emitAsmInst $
        X86.Mov
          { X86.src = X86.Reg X86.Rax,
            X86.dst = X86.Mem X86.Rbp offset
          }
    Nothing -> error $ "Variable " ++ show varId ++ " not found in stack frame"

-- All temps pass by rax register
pushTempToStack :: State TranslationState ()
pushTempToStack = emitAsmInst X86.Push {X86.op = X86.Reg X86.Rax}

popTempFromStack :: State TranslationState ()
popTempFromStack = emitAsmInst X86.Pop {X86.op = X86.Reg X86.Rax}

translateInst :: Mir.Types.Inst -> State TranslationState ()
translateInst inst
  | Mir.Types.Assign _ operand <- inst = do
      -- Translate assignment instruction
      case operand of
        Mir.Types.ConstInt value -> do
          emitAsmInst $ X86.Push {X86.op = X86.Imm value}
          return ()
        Mir.Types.Temp _ -> do
          -- Assuming tempOperand is already in rax
          pushTempToStack
          return ()
  | Mir.Types.BinOp _ op _ _ <- inst = do
      emitAsmInst X86.Pop {X86.op = X86.Reg X86.Rbx} -- Pop the second operand into rbx
      emitAsmInst X86.Pop {X86.op = X86.Reg X86.Rax} -- Pop the first operand into rax
      case op of
        Ast.Types.Add -> do
          emitAsmInst X86.Add {X86.src = X86.Reg X86.Rbx, X86.dst = X86.Reg X86.Rax}
          pushTempToStack
        Ast.Types.Sub -> do
          emitAsmInst X86.Sub {X86.src = X86.Reg X86.Rbx, X86.dst = X86.Reg X86.Rax}
          pushTempToStack
        Ast.Types.Mul -> do
          emitAsmInst X86.Imul {X86.src = X86.Reg X86.Rbx, X86.dst = X86.Reg X86.Rax}
          pushTempToStack
        Ast.Types.Equal -> do
          emitAsmInst X86.Cmp {X86.src = X86.Reg X86.Rbx, X86.dst = X86.Reg X86.Rax}
          changeFlagOp X86.Je
        Ast.Types.NotEqual -> do
          emitAsmInst X86.Cmp {X86.src = X86.Reg X86.Rbx, X86.dst = X86.Reg X86.Rax}
          changeFlagOp X86.Jne
        Ast.Types.LessThan -> do
          emitAsmInst X86.Cmp {X86.src = X86.Reg X86.Rbx, X86.dst = X86.Reg X86.Rax}
          changeFlagOp X86.Jl
        Ast.Types.LessThanOrEqual -> do
          emitAsmInst X86.Cmp {X86.src = X86.Reg X86.Rbx, X86.dst = X86.Reg X86.Rax}
          changeFlagOp X86.Jle
        Ast.Types.GreaterThan -> do
          emitAsmInst X86.Cmp {X86.src = X86.Reg X86.Rbx, X86.dst = X86.Reg X86.Rax}
          changeFlagOp X86.Jg
        Ast.Types.GreaterThanOrEqual -> do
          emitAsmInst X86.Cmp {X86.src = X86.Reg X86.Rbx, X86.dst = X86.Reg X86.Rax}
          changeFlagOp X86.Jge

      return ()
  | Mir.Types.Load _ var <- inst = do
      loadVarToEax $ Mir.Types.varId var
      pushTempToStack
      return ()
  | Mir.Types.Store var _ <- inst = do
      popTempFromStack
      storeEaxToVar $ Mir.Types.varId var
      return ()
  | Mir.Types.Call _ id _ <- inst =
      emitAsmInst $ X86.Call {X86.name = id}
  | Mir.Types.Param _ <- inst = do
      -- pushTempToStack
      return ()
  | Mir.Types.Return _ <- inst =
      emitAsmInst X86.Ret
  | Mir.Types.Jump label <- inst = do
      emitAsmInst $ X86.Jmp {X86.label = label}
  | Mir.Types.CondJump _ label1 label2 <- inst = do
      jmpInstruction <-
        flagOp >>= \case
          Just cond -> return $ X86.JmpCond {X86.cond = cond, X86.label = label1}
          Nothing -> error "No flag changing operation before conditional jump"
      emitAsmInst jmpInstruction
      emitAsmInst $ X86.Jmp {X86.label = label2}

translateBasicBlock :: Mir.Types.BasicBlock -> State TranslationState ()
translateBasicBlock (Mir.Types.BasicBlock label insts) = do
  emitAsmInst X86.Label {X86.label = label}
  mapM_ translateInst insts

translateFun :: Mir.Types.Fun -> State TranslationState ()
translateFun (Mir.Types.Fun id args locals _ basicBlocks) = do
  -- Allocate space for local variables
  let stackFrameSize = length locals * 8 -- Assuming each argument and local variable is a QWORD (8 bytes)
  mapM_ emitAsmInst (X86.functionPrologue id stackFrameSize)

  -- from rbp:
  -- at position 0 is the last rbp
  -- at position 8 is the return address
  -- at position 16 is the first argument, the second argument is at position 24, etc.
  let argOffsets = zip args (iterate (+ 8) 16)

  -- local variables have negative offsets from rbp
  -- at position -8 is the first local variable, at position -16 is the second local variable, etc.
  let varOffsets = zip locals (iterate (\x -> x - 8) (-8))

  let stackFrameVars = Data.Map.fromList $ argOffsets ++ varOffsets
  modify (\s -> s {stackFrameVars})

  mapM_ translateBasicBlock (reverse basicBlocks)

  mapM_ emitAsmInst X86.functionEpilogue

tranlateMainFun :: Mir.Types.Fun -> State TranslationState ()
tranlateMainFun (Mir.Types.Fun _ _ locals _ basicBlocks) = do
  let frameSize = length locals * 8
  mapM_ emitAsmInst (X86.mainFunctionPrologue frameSize)

  -- here 0 is the first local variable, -8 is the second, etc.
  let varOffsets = zip locals (iterate (\x -> x - 8) 0)
  let stackFrameVars = Data.Map.fromList varOffsets
  modify (\s -> s {stackFrameVars})

  mapM_ translateBasicBlock (reverse basicBlocks)

  mapM_ emitAsmInst X86.mainFunctionEpilogue

translateProgram' :: Mir.Types.Program -> State TranslationState ()
translateProgram' (Mir.Types.Program funs externFuns mainFun) = do
  modify
    ( \s ->
        s
          { fileHeader =
              X86.fileHeader ((\Mir.Types.ExternFun {Mir.Types.externId} -> externId) <$> externFuns)
          }
    )

  -- translate main function
  case mainFun of
    Just fun -> tranlateMainFun fun
    Nothing -> return ()
  -- translate functions
  mapM_ translateFun funs

translateProgram :: Mir.Types.Program -> String
translateProgram program = show finalState
  where
    initialState =
      TranslationState
        { stackFrameVars = empty,
          assemblyCode = [],
          lastJmpCond = Nothing,
          fileHeader = ""
        }
    (_, finalState) = runState (translateProgram' program) initialState

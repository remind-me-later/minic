{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module X86.Translate where

import Ast.Types qualified
import Control.Monad.State (State, get, modify, runState)
import Data.Map (Map, empty, fromList, lookup)
import Mir.Types qualified

data IfCond
  = NZ
  | Z
  | EQ
  | LT
  | LE
  | GT
  | GE
  deriving (Show, Eq)

ifCondToJmpInstruction :: IfCond -> String
ifCondToJmpInstruction X86.Translate.NZ = "jnz"
ifCondToJmpInstruction X86.Translate.Z = "jz"
ifCondToJmpInstruction X86.Translate.EQ = "je"
ifCondToJmpInstruction X86.Translate.LT = "jl"
ifCondToJmpInstruction X86.Translate.LE = "jle"
ifCondToJmpInstruction X86.Translate.GT = "jg"
ifCondToJmpInstruction X86.Translate.GE = "jge"

data TranslationState = TranslationState
  { stackFrameVars :: Map Ast.Types.Id Int,
    stackFrameSize :: Int,
    assemblyCode :: String,
    lastFlagChangingOp :: Maybe IfCond
  }

changeFlagOp :: IfCond -> State TranslationState ()
changeFlagOp cond = modify (\s -> s {lastFlagChangingOp = Just cond})

flagOp :: State TranslationState (Maybe IfCond)
flagOp = do
  TranslationState {lastFlagChangingOp} <- get
  return lastFlagChangingOp

appendAssemblyCode :: String -> State TranslationState ()
appendAssemblyCode code = do
  TranslationState {assemblyCode} <- get
  let newCode = assemblyCode ++ code
  modify (\s -> s {assemblyCode = newCode})

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
      -- Load the variable into rax register, assuming rax is used for variable access,
      -- and variable is stored at offset from rbp
      appendAssemblyCode $ "mov rax, [rbp - " ++ show offset ++ "]\n"
    Nothing -> error $ "Variable " ++ show varId ++ " not found in stack frame"

storeEaxToVar :: Ast.Types.Id -> State TranslationState ()
storeEaxToVar varId = do
  maybeOffset <- getVarOffset varId
  case maybeOffset of
    Just offset -> appendAssemblyCode $ "mov [rbp - " ++ show offset ++ "], rax\n"
    Nothing -> error $ "Variable " ++ show varId ++ " not found in stack frame"

-- All temps pass by rax register
pushTempToStack :: State TranslationState ()
pushTempToStack = appendAssemblyCode "push rax\n"

popTempFromStack :: State TranslationState ()
popTempFromStack = appendAssemblyCode "pop rax\n"

constToEax :: Int -> State TranslationState ()
constToEax value = appendAssemblyCode $ "mov rax, " ++ show value ++ "\n"

translateInst :: Mir.Types.Inst -> State TranslationState ()
translateInst inst
  | Mir.Types.Assign _ operand <- inst = do
      -- Translate assignment instruction
      case operand of
        Mir.Types.ConstInt value -> do
          constToEax value
          pushTempToStack
          return ()
        Mir.Types.Temp _ -> do
          -- Assuming tempOperand is already in rax
          pushTempToStack
          return ()
  | Mir.Types.BinOp _ op _ _ <- inst = do
      -- Translate binary operation
      popTempFromStack -- Pop the second operand into rax
      -- move the second operand to rbx
      appendAssemblyCode "mov rbx, rax\n"
      popTempFromStack -- Pop the first operand into rax
      case op of
        Ast.Types.Add -> do
          appendAssemblyCode "add rax, rbx\n"
          pushTempToStack
        Ast.Types.Sub -> do
          appendAssemblyCode "sub rax, rbx\n"
          pushTempToStack
        Ast.Types.Mul -> do
          appendAssemblyCode "imul rax, rbx\n"
          pushTempToStack
        Ast.Types.Equal -> do
          appendAssemblyCode "cmp rax, rbx\n"
          changeFlagOp X86.Translate.EQ
        Ast.Types.NotEqual -> do
          appendAssemblyCode "cmp rax, rbx\n"
          changeFlagOp X86.Translate.NZ
        Ast.Types.LessThan -> do
          appendAssemblyCode "cmp rax, rbx\n"
          changeFlagOp X86.Translate.LT
        Ast.Types.LessThanOrEqual -> do
          appendAssemblyCode "cmp rax, rbx\n"
          changeFlagOp X86.Translate.LE
        Ast.Types.GreaterThan -> do
          appendAssemblyCode "cmp rax, rbx\n"
          changeFlagOp X86.Translate.GT
        Ast.Types.GreaterThanOrEqual -> do
          appendAssemblyCode "cmp rax, rbx\n"
          changeFlagOp X86.Translate.GE

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
      appendAssemblyCode $ "call " ++ id ++ "\n"
  | Mir.Types.Param _ <- inst = do
      -- pushTempToStack
      return ()
  | Mir.Types.Return _ <- inst =
      appendAssemblyCode "ret\n"
  | Mir.Types.Jump label <- inst = do
      appendAssemblyCode $ "jmp " ++ label ++ "\n"
  | Mir.Types.CondJump _ label1 label2 <- inst = do
      jmpInstruction <-
        flagOp >>= \case
          Just cond -> return $ ifCondToJmpInstruction cond ++ " " ++ label1 ++ "\n"
          Nothing -> error "No flag changing operation before conditional jump"
      appendAssemblyCode jmpInstruction
      appendAssemblyCode $ "jmp " ++ label2 ++ "\n"

translateBasicBlock :: Mir.Types.BasicBlock -> State TranslationState ()
translateBasicBlock (Mir.Types.BasicBlock label insts) = do
  appendAssemblyCode $ label ++ ":\n"
  mapM_ translateInst insts

translateFun :: Mir.Types.Fun -> State TranslationState ()
translateFun (Mir.Types.Fun id args locals _ basicBlocks) = do
  appendAssemblyCode $ "\n" ++ id ++ ":\n"

  -- Initialize stack frame
  appendAssemblyCode "push rbp\n"
  appendAssemblyCode "mov rbp, rsp\n"

  -- Allocate space for local variables
  let argCount = length args
  let localCount = length locals
  let stackFrameSize = (argCount + localCount) * 8 -- Assuming each argument and local variable is a QWORD (8 bytes)
  modify (\s -> s {stackFrameSize})
  appendAssemblyCode $ "sub rsp, " ++ show stackFrameSize ++ "\n"
  -- Initialize stack frame variables
  let stackFrameVars = Data.Map.fromList $ zip (args ++ locals) [0, 8 .. (stackFrameSize - 8)]
  modify (\s -> s {stackFrameVars})

  -- Translate each basic block
  mapM_
    ( \bb -> do
        translateBasicBlock bb
        appendAssemblyCode "\n"
    )
    (reverse basicBlocks)

  -- Restore stack frame
  appendAssemblyCode "pop rbp\n"
  appendAssemblyCode "ret\n"

tranlateMainFun :: Mir.Types.Fun -> State TranslationState ()
tranlateMainFun (Mir.Types.Fun _ args locals _ basicBlocks) = do
  appendAssemblyCode "\n_start:\n"

  -- Allocate space for local variables
  let argCount = length args
  let localCount = length locals
  let stackFrameSize = (argCount + localCount) * 8 -- Assuming each argument and local variable is a QWORD (8 bytes)
  modify (\s -> s {stackFrameSize})
  -- Initialize stack frame variables
  let stackFrameVars = Data.Map.fromList $ zip (args ++ locals) [0, 8 .. (stackFrameSize - 8)]
  modify (\s -> s {stackFrameVars})
  appendAssemblyCode $ "sub rsp, " ++ show stackFrameSize ++ "\n"

  -- Translate each basic block
  mapM_
    ( \bb -> do
        translateBasicBlock bb
        appendAssemblyCode "\n"
    )
    (reverse basicBlocks)

  appendAssemblyCode "mov rax, 60\n" -- syscall number for sys_exit
  appendAssemblyCode "xor rdi, rdi\n" -- exit code 0
  appendAssemblyCode "syscall\n" -- exit the program

translateProgram' :: Mir.Types.Program -> State TranslationState ()
translateProgram' (Mir.Types.Program funs externFuns mainFun) = do
  -- add global
  appendAssemblyCode "global _start\n"
  appendAssemblyCode "section .text\n"
  -- translate extern functions
  mapM_ (\Mir.Types.ExternFun {Mir.Types.externId} -> appendAssemblyCode $ "extern " ++ externId ++ "\n") externFuns
  -- translate main function
  case mainFun of
    Just fun -> tranlateMainFun fun
    Nothing -> return ()
  -- translate functions
  mapM_ translateFun funs

translateProgram :: Mir.Types.Program -> String
translateProgram program = finalState.assemblyCode
  where
    initialState =
      TranslationState
        { stackFrameVars = empty,
          stackFrameSize = 0,
          assemblyCode = "",
          lastFlagChangingOp = Nothing
        }
    (_, finalState) = runState (translateProgram' program) initialState

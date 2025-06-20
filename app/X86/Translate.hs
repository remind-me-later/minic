{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module X86.Translate where

import Ast.Types qualified
import Control.Monad.State (State, get, modify, runState)
import Data.Map (Map, empty, fromList, lookup)
import Mir.Types qualified

data TranslationState = TranslationState
  { stackFrameVars :: Map Ast.Types.Id Int,
    stackFrameSize :: Int,
    assemblyCode :: String
  }

appendAssemblyCode :: String -> State TranslationState ()
appendAssemblyCode code = do
  TranslationState {assemblyCode} <- get
  let newCode = assemblyCode ++ code
  modify (\s -> s {assemblyCode = newCode})

-- Offset from rbp (base pointer) for local variables
-- All variables are DWORDS and aligned
getVarOffset :: Ast.Types.Id -> State TranslationState (Maybe Int)
getVarOffset varId = do
  TranslationState {stackFrameVars} <- get
  return $ Data.Map.lookup varId stackFrameVars

loadVarToEax :: Ast.Types.Id -> State TranslationState ()
loadVarToEax varId = do
  maybeOffset <- getVarOffset varId
  case maybeOffset of
    Just offset ->
      -- Load the variable into eax register, assuming eax is used for variable access,
      -- and variable is stored at offset from rbp
      appendAssemblyCode $ "mov eax, DWORD PTR [rbp - " ++ show offset ++ "]\n"
    Nothing -> error $ "Variable " ++ show varId ++ " not found in stack frame"

storeEaxToVar :: Ast.Types.Id -> State TranslationState ()
storeEaxToVar varId = do
  maybeOffset <- getVarOffset varId
  case maybeOffset of
    Just offset -> appendAssemblyCode $ "mov DWORD PTR [rbp - " ++ show offset ++ "], eax\n"
    Nothing -> error $ "Variable " ++ show varId ++ " not found in stack frame"

-- All temps pass by eax register
pushTempToStack :: State TranslationState ()
pushTempToStack = appendAssemblyCode "push eax\n"

popTempFromStack :: State TranslationState ()
popTempFromStack = appendAssemblyCode "pop eax\n"

constToEax :: Int -> State TranslationState ()
constToEax value = appendAssemblyCode $ "mov eax, " ++ show value ++ "\n"

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
          -- Assuming tempOperand is already in eax
          pushTempToStack
          return ()
  | Mir.Types.BinOp _ op _ _ <- inst = do
      -- Translate binary operation
      popTempFromStack -- Pop the second operand into eax
      -- move the second operand to ebx
      appendAssemblyCode "mov ebx, eax\n"
      popTempFromStack -- Pop the first operand into eax
      case op of
        Ast.Types.Add -> appendAssemblyCode "add eax, ebx\n"
        Ast.Types.Sub -> appendAssemblyCode "sub eax, ebx\n"
        Ast.Types.Mul -> appendAssemblyCode "imul eax, ebx\n"
        Ast.Types.Equal -> appendAssemblyCode "cmp eax, ebx\n"
        Ast.Types.NotEqual -> appendAssemblyCode "cmp eax, ebx\n"
        Ast.Types.LessThan -> appendAssemblyCode "cmp eax, ebx\n"
        Ast.Types.LessThanOrEqual -> appendAssemblyCode "cmp eax, ebx\n"
        Ast.Types.GreaterThan -> appendAssemblyCode "cmp eax, ebx\n"
        Ast.Types.GreaterThanOrEqual -> appendAssemblyCode "cmp eax, ebx\n"

      pushTempToStack -- Push the result back to stack
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
      pushTempToStack
      return ()
  | Mir.Types.Return _ <- inst =
      appendAssemblyCode "ret\n"
  | Mir.Types.Jump label <- inst = do
      appendAssemblyCode $ "jmp " ++ label ++ "\n"
  | Mir.Types.CondJump _ label1 label2 <- inst = do
      popTempFromStack -- Pop the condition into eax
      -- appendAssemblyCode $ "cmp eax, 0\n" -- Assuming eax contains the condition
      appendAssemblyCode $ "je " ++ label2 ++ "\n"
      appendAssemblyCode $ "jmp " ++ label1 ++ "\n"

translateBasicBlock :: Mir.Types.BasicBlock -> State TranslationState ()
translateBasicBlock (Mir.Types.BasicBlock label insts) = do
  appendAssemblyCode $ label ++ ":\n"
  mapM_ translateInst insts

translateFun :: Mir.Types.Fun -> State TranslationState ()
translateFun (Mir.Types.Fun id args locals _ basicBlocks) = do
  appendAssemblyCode $ id ++ ":\n"
  -- Initialize stack frame
  appendAssemblyCode "push rbp\n"
  appendAssemblyCode "mov rbp, rsp\n"

  -- Allocate space for local variables
  let argCount = length args
  let localCount = length locals
  let stackFrameSize = (argCount + localCount) * 4 -- Assuming each argument and local variable is a DWORD (4 bytes)
  modify (\s -> s {stackFrameSize})
  appendAssemblyCode $ "sub rsp, " ++ show stackFrameSize ++ "\n"
  -- Initialize stack frame variables
  let stackFrameVars = Data.Map.fromList $ zip (args ++ locals) [0, 4 .. (stackFrameSize - 4)]
  modify (\s -> s {stackFrameVars})

  -- Translate each basic block
  mapM_ translateBasicBlock (reverse basicBlocks)

  -- Restore stack frame
  appendAssemblyCode "pop rbp\n"
  appendAssemblyCode "ret\n"

translateProgram :: Mir.Types.Program -> String
translateProgram (Mir.Types.Program funs) =
  let initialState = TranslationState Data.Map.empty 0 ""
      (_, finalState) = runState (mapM translateFun funs) initialState
   in assemblyCode finalState
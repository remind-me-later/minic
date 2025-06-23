{-# OPTIONS_GHC -Wno-name-shadowing #-}

module X86.Translate where

import Ast.Types qualified as Ast
import Control.Monad (forM_)
import Control.Monad.State (State, gets, modify, runState)
import Data.List (iterate')
import Data.Map (Map, empty, fromList, lookup)
import Mir.Types qualified as Mir
import X86.Types

functionPrologue :: String -> Int -> [Inst]
functionPrologue name frameSize =
  [ Label {label = name},
    Push {op = Reg Rbp}, -- Save base pointer
    Mov {src = Reg Rsp, dst = Reg Rbp}, -- Set base pointer to current stack pointer
    Sub {src = Imm frameSize, dst = Reg Rsp} -- Allocate stack frame
  ]

functionEpilogue :: [Inst]
functionEpilogue =
  [ Mov {src = Reg Rbp, dst = Reg Rsp}, -- Restore stack pointer
    Pop {op = Reg Rbp}, -- Restore base pointer
    Ret -- Return from function
  ]

mainFunctionPrologue :: Int -> [Inst]
mainFunctionPrologue frameSize =
  [ Label {label = "_start"},
    Mov {src = Reg Rsp, dst = Reg Rbp}, -- Set base pointer to current stack pointer
    Sub {src = Imm frameSize, dst = Reg Rsp} -- Allocate stack frame
  ]

mainFunctionEpilogue :: [Inst]
mainFunctionEpilogue =
  [ Mov {src = Imm 60, dst = Reg Rax}, -- syscall number for sys_exit
    Xor {src = Reg Rdi, dst = Reg Rdi}, -- exit code 0
    Syscall -- exit the program
  ]

fileHeader :: [String] -> String
fileHeader externs =
  "BITS 64\n\n"
    ++ "global _start\n\n"
    ++ "section .text\n"
    ++ concatMap (\e -> "extern " ++ e ++ "\n") externs

data TranslationState = TranslationState
  { varOffsets :: Map Ast.Id Int,
    assemblyCode :: [Inst],
    lastJmpCond :: Maybe JmpCond,
    fileHeader :: String
  }

instance Show TranslationState where
  show ts = ts.fileHeader ++ "\n" ++ unlines (show <$> ts.assemblyCode)

changeFlagOp :: JmpCond -> State TranslationState ()
changeFlagOp cond = modify (\s -> s {lastJmpCond = Just cond})

emitAsmInst :: Inst -> State TranslationState ()
emitAsmInst code = modify (\s -> s {assemblyCode = s.assemblyCode ++ [code]})

-- Offset from rbp (base pointer) for local variables
-- All variables are QWORDS and aligned
getVarOffset :: Ast.Id -> State TranslationState (Maybe Int)
getVarOffset varId = gets (Data.Map.lookup varId . (.varOffsets))

loadVarToEax :: Ast.Id -> State TranslationState ()
loadVarToEax varId = do
  maybeOffset <- getVarOffset varId
  case maybeOffset of
    Just offset ->
      emitAsmInst $
        Mov
          { src = Mem Rbp offset,
            dst = Reg Rax
          }
    Nothing -> error $ "Variable " ++ show varId ++ " not found in stack frame"

storeEaxToVar :: Ast.Id -> State TranslationState ()
storeEaxToVar varId = do
  maybeOffset <- getVarOffset varId
  case maybeOffset of
    Just offset ->
      emitAsmInst $
        Mov
          { src = Reg Rax,
            dst = Mem Rbp offset
          }
    Nothing -> error $ "Variable " ++ show varId ++ " not found in stack frame"

-- All temps pass by rax register
pushTempToStack :: State TranslationState ()
pushTempToStack = emitAsmInst Push {op = Reg Rax}

popTempFromStack :: State TranslationState ()
popTempFromStack = emitAsmInst Pop {op = Reg Rax}

translateInst :: Mir.Inst -> State TranslationState ()
translateInst inst
  | Mir.Assign {srcOp} <- inst = do
      -- Translate assignment instruction
      case srcOp of
        Mir.ConstInt value -> do
          emitAsmInst Push {op = Imm value}
          return ()
        Mir.Temp _ -> do
          -- Assuming tempOperand is already in rax
          pushTempToStack
          return ()
  | Mir.BinOp {binop} <- inst = do
      emitAsmInst Pop {op = Reg Rbx} -- Pop the second operand into rbx
      emitAsmInst Pop {op = Reg Rax} -- Pop the first operand into rax
      case binop of
        Ast.Add -> do
          emitAsmInst Add {src = Reg Rbx, dst = Reg Rax}
          changeFlagOp Jnz -- Add operation changes the flags
          pushTempToStack
        Ast.Sub -> do
          emitAsmInst Sub {src = Reg Rbx, dst = Reg Rax}
          changeFlagOp Jnz -- Sub operation changes the flags
          pushTempToStack
        Ast.Mul -> do
          emitAsmInst Imul {src = Reg Rbx, dst = Reg Rax}
          changeFlagOp Jnz -- Mul operation changes the flags
          pushTempToStack
        Ast.Equal -> do
          emitAsmInst Cmp {src = Reg Rbx, dst = Reg Rax}
          changeFlagOp Je
        Ast.NotEqual -> do
          emitAsmInst Cmp {src = Reg Rbx, dst = Reg Rax}
          changeFlagOp Jne
        Ast.LessThan -> do
          emitAsmInst Cmp {src = Reg Rbx, dst = Reg Rax}
          changeFlagOp Jl
        Ast.LessThanOrEqual -> do
          emitAsmInst Cmp {src = Reg Rbx, dst = Reg Rax}
          changeFlagOp Jle
        Ast.GreaterThan -> do
          emitAsmInst Cmp {src = Reg Rbx, dst = Reg Rax}
          changeFlagOp Jg
        Ast.GreaterThanOrEqual -> do
          emitAsmInst Cmp {src = Reg Rbx, dst = Reg Rax}
          changeFlagOp Jge
        Ast.And -> do
          emitAsmInst And {src = Reg Rbx, dst = Reg Rax}
          changeFlagOp Jnz -- And operation changes the flags
          pushTempToStack
        Ast.Or -> do
          emitAsmInst Or {src = Reg Rbx, dst = Reg Rax}
          changeFlagOp Jnz -- Or operation changes the flags
          pushTempToStack
        Ast.Xor -> do
          emitAsmInst Xor {src = Reg Rbx, dst = Reg Rax}
          changeFlagOp Jnz -- Xor operation changes the flags
          pushTempToStack
        Ast.Modulo -> do
          emitAsmInst Mod {src = Reg Rbx, dst = Reg Rax}
          changeFlagOp Jnz -- Modulo operation changes the flags
          pushTempToStack

      return ()
  | Mir.UnaryOp {unop} <- inst = do
      emitAsmInst Pop {op = Reg Rax} -- Pop the operand into rax
      case unop of
        Ast.UnarySub -> do
          emitAsmInst Neg {op = Reg Rax}
          changeFlagOp Jnz -- Negation changes the flags
          pushTempToStack
        Ast.UnaryNot -> do
          emitAsmInst Not {op = Reg Rax}
          changeFlagOp Jz -- Not operation changes the flags
          pushTempToStack
      return ()
  | Mir.Load {srcVar} <- inst = do
      loadVarToEax srcVar.id
      pushTempToStack
      return ()
  | Mir.Store {dstVar} <- inst = do
      popTempFromStack
      storeEaxToVar dstVar.id
      return ()
  | Mir.Call {funId, ret} <- inst = do
      emitAsmInst $ Call {name = funId}
      case ret of
        Just {} -> do
          -- push the return value to the stack
          pushTempToStack
        Nothing -> return ()
  | Mir.Param {} <- inst = do
      return ()
  | Mir.Return {} <- inst = do
      -- Return value goes to rax
      popTempFromStack
      mapM_ emitAsmInst functionEpilogue
  | Mir.Jump {target} <- inst = emitAsmInst $ Jmp {label = target}
  | Mir.CondJump {trueLabel, falseLabel} <- inst = do
      jmpInstruction <-
        (gets (.lastJmpCond)) >>= \case
          Just cond -> return $ JmpCond {cond = cond, label = trueLabel}
          Nothing -> error "No flag changing operation before conditional jump"
      emitAsmInst jmpInstruction
      emitAsmInst $ Jmp {label = falseLabel}

translateBasicBlock :: Mir.BasicBlock -> State TranslationState ()
translateBasicBlock (Mir.BasicBlock label insts) = do
  emitAsmInst Label {label = label}
  mapM_ translateInst insts

translateFun :: Mir.Fun -> State TranslationState ()
translateFun Mir.Fun {id, args, locals, blocks} = do
  -- Allocate space for local variables
  let stackFrameSize = length locals * 8 -- Assuming each argument and local variable is a QWORD (8 bytes)
  mapM_ emitAsmInst (functionPrologue id stackFrameSize)

  -- from rbp:
  -- at position 0 is the last rbp
  -- at position 8 is the return address
  -- at position 16 is the first argument, the second argument is at position 24, etc.
  let argOffsets = zip args (iterate' (+ 8) 16)

  -- local variables have negative offsets from rbp
  -- at position -8 is the first local variable, at position -16 is the second local variable, etc.
  let varOffsetList = zip locals (iterate' (+ (-8)) 0)

  let varOffsets = Data.Map.fromList $ argOffsets ++ varOffsetList
  modify (\s -> s {varOffsets})

  mapM_ translateBasicBlock (reverse blocks)

  mapM_ emitAsmInst functionEpilogue

tranlateMainFun :: Mir.Fun -> State TranslationState ()
tranlateMainFun Mir.Fun {locals, blocks} = do
  let frameSize = length locals * 8
  mapM_ emitAsmInst (mainFunctionPrologue frameSize)

  -- here 0 is the first local variable, -8 is the second, etc.
  let varOffsetList = zip locals (iterate (+ (-8)) 0)
  let varOffsets = Data.Map.fromList varOffsetList
  modify (\s -> s {varOffsets})

  mapM_ translateBasicBlock (reverse blocks)

  mapM_ emitAsmInst mainFunctionEpilogue

translateProgram' :: Mir.Program -> State TranslationState ()
translateProgram' Mir.Program {funs, externFuns, mainFun} = do
  modify
    ( \s ->
        s
          { fileHeader =
              fileHeader ((\Mir.ExternFun {Mir.externId} -> externId) <$> externFuns)
          }
    )

  -- translate main function
  forM_ mainFun tranlateMainFun
  -- translate functions
  mapM_ translateFun funs

translateProgram :: Mir.Program -> String
translateProgram program = show finalState
  where
    initialState =
      TranslationState
        { varOffsets = empty,
          assemblyCode = [],
          lastJmpCond = Nothing,
          fileHeader = ""
        }
    (_, finalState) = runState (translateProgram' program) initialState

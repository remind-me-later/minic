{-# OPTIONS_GHC -Wno-name-shadowing #-}

module X86.Translate where

import Control.Monad (forM_, unless)
import Control.Monad.State (State, gets, modify', runState)
import Mir.Types qualified as Mir
import TypeSystem qualified (BinOp (..), UnaryOp (..))
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

makeFileHeader :: [String] -> String
makeFileHeader externs =
  ".section .text\n"
    ++ ".globl _start\n\n"
    ++ concatMap
      ( \e ->
          ".extern "
            ++ e
            ++ "\n.type "
            ++ e
            ++ ", @function\n"
      )
      externs

data TranslationState = TranslationState
  { assemblyCode :: [Inst],
    lastJmpCond :: Maybe JmpCond,
    fileHeader :: String
  }

instance Show TranslationState where
  show ts = ts.fileHeader ++ "\n" ++ unlines (show <$> ts.assemblyCode)

changeFlagOp :: JmpCond -> State TranslationState ()
changeFlagOp cond = modify' (\s -> s {lastJmpCond = Just cond})

emitAsmInst :: Inst -> State TranslationState ()
emitAsmInst code = modify' (\s -> s {assemblyCode = s.assemblyCode ++ [code]})

-- Convert MIR register to X86 register
mirRegisterToX86 :: Mir.Register -> Reg
mirRegisterToX86 Mir.R1 = R8
mirRegisterToX86 Mir.R2 = R9
mirRegisterToX86 Mir.R3 = R10
mirRegisterToX86 Mir.R4 = R11
mirRegisterToX86 Mir.R5 = R12
mirRegisterToX86 Mir.R6 = R13
mirRegisterToX86 Mir.R7 = R14
mirRegisterToX86 Mir.R8 = R15

-- Convert MIR operand to X86 operand
translateOperand :: Mir.Operand -> Op
translateOperand (Mir.ConstInt n) = Imm n
translateOperand (Mir.ConstChar c) = Imm (fromIntegral (fromEnum c))
translateOperand (Mir.RegOperand reg) = Reg (mirRegisterToX86 reg)
translateOperand (Mir.StackOperand offset) = Mem {base = Rbp, disp = offset, index_scale = Nothing}
translateOperand (Mir.Temp _) = error "Temporary variables should be handled by register allocation"

calculateFrameSize :: Mir.CFG -> Int
calculateFrameSize cfg =
  let allOperands = concatMap getStackOperands cfg.blocks
      stackOffsets = [abs offset | Mir.StackOperand offset <- allOperands, offset < 0]
   in if null stackOffsets then 0 else maximum stackOffsets

getStackOperands :: Mir.BasicBlock -> [Mir.Operand]
getStackOperands Mir.BasicBlock {insts} = concatMap extractOperands insts
  where
    extractOperands (Mir.Assign {dst, src}) = [dst, src]
    extractOperands (Mir.BinOp {dst, left, right}) = [dst, left, right]
    extractOperands (Mir.UnaryOp {dst, src}) = [dst, src]
    extractOperands (Mir.Call {ret = Just retOperand}) = [retOperand]
    extractOperands (Mir.Call {ret = Nothing}) = []
    extractOperands (Mir.Param {param}) = [param]

-- Load operand value into a specific register
loadOperandToReg :: Mir.Operand -> Reg -> State TranslationState ()
loadOperandToReg operand targetReg = do
  let src = translateOperand operand
  case src of
    Reg srcReg | srcReg == targetReg -> return () -- Already in target register
    _ -> emitAsmInst $ Mov {src, dst = Reg targetReg}

translateInst :: Mir.Inst -> State TranslationState ()
translateInst inst
  | Mir.Assign {dst, src} <- inst = do
      let dstOp = translateOperand dst
      let srcOp = translateOperand src
      case (dstOp, srcOp) of
        (Reg {}, _) -> emitAsmInst $ Mov {src = srcOp, dst = dstOp}
        (Mem {}, Reg {}) -> emitAsmInst $ Mov {src = srcOp, dst = dstOp}
        (Mem {}, _) -> do
          -- Need to go through a register for memory-to-memory moves
          emitAsmInst $ Mov {src = srcOp, dst = Reg Rax}
          emitAsmInst $ Mov {src = Reg Rax, dst = dstOp}
        _ -> error "Invalid assignment operands"
  | Mir.BinOp {dst, binop, left, right} <- inst = do
      let dstOp = translateOperand dst
      loadOperandToReg left Rax
      let rightOp = translateOperand right

      case binop of
        TypeSystem.Add -> do
          emitAsmInst Add {src = rightOp, dst = Reg Rax}
          changeFlagOp Jnz
        TypeSystem.Sub -> do
          emitAsmInst Sub {src = rightOp, dst = Reg Rax}
          changeFlagOp Jnz
        TypeSystem.Mul -> do
          emitAsmInst Imul {src = rightOp, dst = Reg Rax}
          changeFlagOp Jnz
        TypeSystem.Div -> do
          emitAsmInst Cqo

          case rightOp of
            i@Imm {} -> do
              emitAsmInst Mov {src = i, dst = Reg Rbx}
              emitAsmInst Idiv {src = Reg Rbx}
            _ -> emitAsmInst Idiv {src = rightOp}
          changeFlagOp Jnz
        TypeSystem.Equal -> do
          emitAsmInst Cmp {src = rightOp, dst = Reg Rax}
          changeFlagOp Je
        TypeSystem.NotEqual -> do
          emitAsmInst Cmp {src = rightOp, dst = Reg Rax}
          changeFlagOp Jne
        TypeSystem.LessThan -> do
          emitAsmInst Cmp {src = rightOp, dst = Reg Rax}
          changeFlagOp Jl
        TypeSystem.LessThanOrEqual -> do
          emitAsmInst Cmp {src = rightOp, dst = Reg Rax}
          changeFlagOp Jle
        TypeSystem.GreaterThan -> do
          emitAsmInst Cmp {src = rightOp, dst = Reg Rax}
          changeFlagOp Jg
        TypeSystem.GreaterThanOrEqual -> do
          emitAsmInst Cmp {src = rightOp, dst = Reg Rax}
          changeFlagOp Jge
        TypeSystem.And -> do
          emitAsmInst And {src = rightOp, dst = Reg Rax}
          changeFlagOp Jnz
        TypeSystem.Or -> do
          emitAsmInst Or {src = rightOp, dst = Reg Rax}
          changeFlagOp Jnz
        TypeSystem.Xor -> do
          emitAsmInst Xor {src = rightOp, dst = Reg Rax}
          changeFlagOp Jnz
        TypeSystem.Mod -> do
          emitAsmInst Cqo

          case rightOp of
            i@Imm {} -> do
              emitAsmInst Mov {src = i, dst = Reg Rbx}
              emitAsmInst Idiv {src = Reg Rbx}
            _ -> emitAsmInst Idiv {src = rightOp}

          emitAsmInst $ Mov {src = Reg Rdx, dst = Reg Rax} -- Move remainder to result
          changeFlagOp Jnz

      -- Store result to destination
      case dstOp of
        Reg dstReg | dstReg == Rax -> return () -- Already in place
        Reg {} -> emitAsmInst $ Mov {src = Reg Rax, dst = dstOp}
        _ -> emitAsmInst $ Mov {src = Reg Rax, dst = dstOp}
  | Mir.UnaryOp {dst, unop, src} <- inst = do
      let dstOp = translateOperand dst
      loadOperandToReg src Rax

      case unop of
        TypeSystem.UnarySub -> do
          emitAsmInst Neg {op = Reg Rax}
          changeFlagOp Jnz
        TypeSystem.UnaryNot -> do
          emitAsmInst Not {op = Reg Rax}
          changeFlagOp Jz

      -- Store result to destination
      case dstOp of
        Reg dstReg | dstReg == Rax -> return () -- Already in place
        _ -> emitAsmInst $ Mov {src = Reg Rax, dst = dstOp}
  | Mir.Call {funId, argCount, ret} <- inst = do
      emitAsmInst $ Call {name = funId}
      -- Remove arguments from the stack (if using stack calling convention)
      forM_ [1 .. argCount] $ \_ -> emitAsmInst Pop {op = Reg Rbx}
      case ret of
        Just retOperand -> do
          let retOp = translateOperand retOperand
          case retOp of
            Reg retReg | retReg == Rax -> return () -- Already in place
            _ -> emitAsmInst $ Mov {src = Reg Rax, dst = retOp}
        Nothing -> return ()
  | Mir.Param {param} <- inst = do
      let paramOp = translateOperand param
      emitAsmInst $ Push {op = paramOp}

translateTerminator :: Mir.Terminator -> State TranslationState ()
translateTerminator terminator
  | Mir.Return {retVal} <- terminator = do
      case retVal of
        Just retTemp -> do
          loadOperandToReg retTemp Rax
          return ()
        Nothing -> return ()
      mapM_ emitAsmInst functionEpilogue
  | Mir.Jump {target} <- terminator = emitAsmInst $ Jmp {label = target}
  | Mir.CondJump {trueBlockId, falseBlockId} <- terminator = do
      jmpInstruction <-
        (gets (.lastJmpCond)) >>= \case
          Just condType -> return $ JmpCond {cond = condType, label = trueBlockId}
          Nothing -> error "No flag changing operation before conditional jump"
      emitAsmInst jmpInstruction
      emitAsmInst $ Jmp {label = falseBlockId}

translateBasicBlock :: Bool -> Bool -> Mir.BasicBlock -> State TranslationState ()
translateBasicBlock isEntryPoint isMain Mir.BasicBlock {blockId, insts, terminator} = do
  unless isEntryPoint $ do
    emitAsmInst Label {label = blockId}
  mapM_ translateInst insts
  unless isMain $ translateTerminator terminator

translateCfg :: Bool -> Mir.CFG -> State TranslationState ()
translateCfg isMain Mir.CFG {blocks} = do
  -- FIXME: should not rely on basic block order
  let entryBlock = head blocks
  translateBasicBlock True isMain entryBlock
  forM_ (tail blocks) $ \block ->
    translateBasicBlock False isMain block

translateMainFun :: Mir.Fun -> State TranslationState ()
translateMainFun Mir.Fun {cfg} = do
  mapM_ emitAsmInst (mainFunctionPrologue (calculateFrameSize cfg))

  translateCfg True cfg

  mapM_ emitAsmInst mainFunctionEpilogue

translateFun :: Mir.Fun -> State TranslationState ()
translateFun Mir.Fun {id, cfg} = do
  mapM_ emitAsmInst (functionPrologue id (calculateFrameSize cfg))

  -- No need to calculate offsets - they're in the StackOperands
  translateCfg False cfg

translateProgram' :: Mir.Program -> State TranslationState ()
translateProgram' Mir.Program {funs, externFuns, mainFun} = do
  modify' (\s -> s {fileHeader = makeFileHeader ((.externId) <$> externFuns)})

  -- translate main function
  forM_ mainFun translateMainFun
  -- translate functions
  mapM_ translateFun funs

translateProgram :: Mir.Program -> String
translateProgram program = show finalState
  where
    initialState =
      TranslationState
        { assemblyCode = [],
          lastJmpCond = Nothing,
          fileHeader = ""
        }

    (_, finalState) = runState (translateProgram' program) initialState

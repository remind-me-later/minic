{-# OPTIONS_GHC -Wno-name-shadowing #-}

module X86.Translate where

import Control.Monad (forM_, unless, when)
import Control.Monad.State (State, gets, modify', runState)
import Mir.Types qualified as Mir
import TypeSystem qualified (BinOp (..), UnaryOp (..))
import X86.Types

functionPrologue :: String -> Int -> [Inst]
functionPrologue name frameSize =
  [ Label {labelName = name},
    Push {pushOp = Reg Rbp}, -- Save base pointer
    Mov {movSrc = Reg Rsp, movDst = Reg Rbp}, -- Set base pointer to current stack pointer
    Sub {subSrc = Imm frameSize, subDst = Reg Rsp} -- Allocate stack frame
  ]

functionEpilogue :: [Inst]
functionEpilogue =
  [ Mov {movSrc = Reg Rbp, movDst = Reg Rsp}, -- Restore stack pointer
    Pop {popOp = Reg Rbp}, -- Restore base pointer
    Ret -- Return from function
  ]

mainFunctionPrologue :: Int -> [Inst]
mainFunctionPrologue frameSize =
  [ Label {labelName = "_start"},
    Mov {movSrc = Reg Rsp, movDst = Reg Rbp}, -- Set base pointer to current stack pointer
    Sub {subSrc = Imm frameSize, subDst = Reg Rsp} -- Allocate stack frame
  ]

mainFunctionEpilogue :: [Inst]
mainFunctionEpilogue =
  [ Mov {movSrc = Imm 60, movDst = Reg Rax}, -- syscall number for sys_exit
    Xor {xorSrc = Reg Rdi, xorDst = Reg Rdi}, -- exit code 0
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
  show TranslationState {assemblyCode, lastJmpCond, fileHeader} =
    fileHeader
      ++ concatMap (\inst -> show inst ++ "\n") assemblyCode
      ++ case lastJmpCond of
        Just cond -> "Last jump condition: " ++ show cond ++ "\n"
        Nothing -> "No last jump condition\n"

changeFlagOp :: JmpCond -> State TranslationState ()
changeFlagOp cond = modify' (\s -> s {lastJmpCond = Just cond})

emitAsmInst :: Inst -> State TranslationState ()
emitAsmInst code =
  modify' (\s@TranslationState {assemblyCode} -> s {assemblyCode = assemblyCode ++ [code]})

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
translateOperand (Mir.StackOperand offset) = Mem {memBase = Rbp, memDisp = offset, memIndexScale = Nothing}
translateOperand (Mir.TempOperand _) = error "Temporary variables should be handled by register allocation"

calculateFrameSize :: Mir.CFG -> Int
calculateFrameSize cfg =
  let allOperands = concatMap getStackOperands (Mir.cfgBlocks cfg)
      stackOffsets = [abs offset | Mir.StackOperand offset <- allOperands, offset < 0]
   in if null stackOffsets then 0 else maximum stackOffsets

getStackOperands :: Mir.BasicBlock -> [Mir.Operand]
getStackOperands Mir.BasicBlock {Mir.blockInsts} = concatMap extractOperands blockInsts
  where
    extractOperands (Mir.Assign {Mir.instDst, Mir.instSrc}) = [instDst, instSrc]
    extractOperands (Mir.BinOp {Mir.instDst, Mir.instLeft, Mir.instRight}) = [instDst, instLeft, instRight]
    extractOperands (Mir.UnaryOp {Mir.instDst, Mir.instSrc}) = [instDst, instSrc]
    extractOperands (Mir.Call {Mir.callRet = Just retOperand}) = [retOperand]
    extractOperands (Mir.Call {Mir.callRet = Nothing}) = []
    extractOperands (Mir.Param {Mir.paramOperand}) = [paramOperand]

-- Load operand value into a specific register
loadOperandToReg :: Mir.Operand -> Reg -> State TranslationState ()
loadOperandToReg operand targetReg = do
  let src = translateOperand operand
  case src of
    Reg srcReg | srcReg == targetReg -> return () -- Already in target register
    _ -> emitAsmInst $ Mov {movSrc = src, movDst = Reg targetReg}

translateInst :: Mir.Inst -> State TranslationState ()
translateInst inst
  | Mir.Assign {Mir.instDst, Mir.instSrc} <- inst = do
      let dstOp = translateOperand instDst
      let srcOp = translateOperand instSrc
      case (dstOp, srcOp) of
        (Reg {}, _) -> emitAsmInst $ Mov {movSrc = srcOp, movDst = dstOp}
        (Mem {}, Reg {}) -> emitAsmInst $ Mov {movSrc = srcOp, movDst = dstOp}
        (Mem {}, _) -> do
          -- Need to go through a register for memory-to-memory moves
          emitAsmInst $ Mov {movSrc = srcOp, movDst = Reg Rax}
          emitAsmInst $ Mov {movSrc = Reg Rax, movDst = dstOp}
        _ -> error "Invalid assignment operands"
  | Mir.BinOp {Mir.instDst, Mir.instBinop, Mir.instLeft, Mir.instRight} <- inst = do
      let dstOp = translateOperand instDst
      loadOperandToReg instLeft Rax
      let rightOp = translateOperand instRight

      case instBinop of
        TypeSystem.Add -> do
          emitAsmInst Add {addSrc = rightOp, addDst = Reg Rax}
          changeFlagOp Jnz
        TypeSystem.Sub -> do
          emitAsmInst Sub {subSrc = rightOp, subDst = Reg Rax}
          changeFlagOp Jnz
        TypeSystem.Mul -> do
          emitAsmInst Imul {imulSrc = rightOp, imulDst = Reg Rax}
          changeFlagOp Jnz
        TypeSystem.Div -> do
          emitAsmInst Cqo

          case rightOp of
            i@Imm {} -> do
              emitAsmInst Mov {movSrc = i, movDst = Reg Rbx}
              emitAsmInst Idiv {idivSrc = Reg Rbx}
            _ -> emitAsmInst Idiv {idivSrc = rightOp}
          changeFlagOp Jnz
        TypeSystem.Equal -> do
          emitAsmInst Cmp {cmpSrc = rightOp, cmpDst = Reg Rax}
          changeFlagOp Je
        TypeSystem.NotEqual -> do
          emitAsmInst Cmp {cmpSrc = rightOp, cmpDst = Reg Rax}
          changeFlagOp Jne
        TypeSystem.LessThan -> do
          emitAsmInst Cmp {cmpSrc = rightOp, cmpDst = Reg Rax}
          changeFlagOp Jl
        TypeSystem.LessThanOrEqual -> do
          emitAsmInst Cmp {cmpSrc = rightOp, cmpDst = Reg Rax}
          changeFlagOp Jle
        TypeSystem.GreaterThan -> do
          emitAsmInst Cmp {cmpSrc = rightOp, cmpDst = Reg Rax}
          changeFlagOp Jg
        TypeSystem.GreaterThanOrEqual -> do
          emitAsmInst Cmp {cmpSrc = rightOp, cmpDst = Reg Rax}
          changeFlagOp Jge
        TypeSystem.And -> do
          emitAsmInst And {andSrc = rightOp, andDst = Reg Rax}
          changeFlagOp Jnz
        TypeSystem.Or -> do
          emitAsmInst Or {orSrc = rightOp, orDst = Reg Rax}
          changeFlagOp Jnz
        TypeSystem.Xor -> do
          emitAsmInst Xor {xorSrc = rightOp, xorDst = Reg Rax}
          changeFlagOp Jnz
        TypeSystem.Mod -> do
          emitAsmInst Cqo

          case rightOp of
            i@Imm {} -> do
              emitAsmInst Mov {movSrc = i, movDst = Reg Rbx}
              emitAsmInst Idiv {idivSrc = Reg Rbx}
            _ -> emitAsmInst Idiv {idivSrc = rightOp}

          emitAsmInst $ Mov {movSrc = Reg Rdx, movDst = Reg Rax} -- Move remainder to result
          changeFlagOp Jnz

      -- Store result to destination
      case dstOp of
        Reg dstReg | dstReg == Rax -> return () -- Already in place
        Reg {} -> emitAsmInst $ Mov {movSrc = Reg Rax, movDst = dstOp}
        _ -> emitAsmInst $ Mov {movSrc = Reg Rax, movDst = dstOp}
  | Mir.UnaryOp {Mir.instDst, Mir.instUnop, Mir.instSrc} <- inst = do
      let dstOp = translateOperand instDst
      loadOperandToReg instSrc Rax

      case instUnop of
        TypeSystem.UnarySub -> do
          emitAsmInst Neg {negOp = Reg Rax}
          changeFlagOp Jnz
        TypeSystem.UnaryNot -> do
          emitAsmInst Not {notOp = Reg Rax}
          changeFlagOp Jz
        
      -- Store result to destination
      case dstOp of
        Reg dstReg | dstReg == Rax -> return () -- Already in place
        _ -> emitAsmInst $ Mov {movSrc = Reg Rax, movDst = dstOp}
  | Mir.Call {Mir.callFunId, Mir.callArgCount, Mir.callRet} <- inst = do
      emitAsmInst $ Call {callName = callFunId}
      -- Remove arguments from the stack (using stack calling convention)
      when (callArgCount > 0) $ do
        let stackOffset = 8 * callArgCount -- Each argument is pushed onto the stack
        emitAsmInst $ Add {addSrc = Imm stackOffset, addDst = Reg Rsp} -- Clean up stack after call
      case callRet of
        Just retOperand -> do
          let retOp = translateOperand retOperand
          case retOp of
            Reg retReg | retReg == Rax -> return () -- Already in place
            _ -> emitAsmInst $ Mov {movSrc = Reg Rax, movDst = retOp}
        Nothing -> return ()
  | Mir.Param {Mir.paramOperand} <- inst = do
      let paramOp = translateOperand paramOperand
      emitAsmInst $ Push {pushOp = paramOp}

translateTerminator :: Mir.Terminator -> State TranslationState ()
translateTerminator terminator
  | Mir.Return {Mir.retOperand} <- terminator = do
      case retOperand of
        Just retTemp -> do
          loadOperandToReg retTemp Rax
          return ()
        Nothing -> return ()
      mapM_ emitAsmInst functionEpilogue
  | Mir.Jump {Mir.jumpTarget} <- terminator = emitAsmInst $ Jmp {jmpLabel = jumpTarget}
  | Mir.CondJump {Mir.condTrueBlockId, Mir.condFalseBlockId} <- terminator = do
      lastJmpCond <- gets lastJmpCond
      jmpInstruction <- case lastJmpCond of
        Just condType -> return $ JmpCond {jmpCond = condType, jmpCondLabel = condTrueBlockId}
        Nothing -> error "No flag changing operation before conditional jump"
      emitAsmInst jmpInstruction
      emitAsmInst $ Jmp {jmpLabel = condFalseBlockId}

translateBasicBlock :: Bool -> Bool -> Mir.BasicBlock -> State TranslationState ()
translateBasicBlock isEntryPoint isMain Mir.BasicBlock {Mir.cfgBlockId, Mir.blockInsts, Mir.blockTerminator} = do
  unless isEntryPoint $ do
    emitAsmInst Label {labelName = cfgBlockId}
  mapM_ translateInst blockInsts
  unless isMain $ translateTerminator blockTerminator

translateCfg :: Bool -> Mir.CFG -> State TranslationState ()
translateCfg _ Mir.CFG {Mir.cfgBlocks = []} =
  error "Empty CFG - cannot translate"
translateCfg isMain Mir.CFG {Mir.cfgBlocks = entryBlock : rest} = do
  -- FIXME: should not rely on basic block order
  translateBasicBlock True isMain entryBlock
  forM_ rest $ \block ->
    translateBasicBlock False isMain block

translateMainFun :: Mir.Fun -> State TranslationState ()
translateMainFun Mir.Fun {Mir.funCfg} = do
  mapM_ emitAsmInst (mainFunctionPrologue (calculateFrameSize funCfg))

  translateCfg True funCfg

  mapM_ emitAsmInst mainFunctionEpilogue

translateFun :: Mir.Fun -> State TranslationState ()
translateFun Mir.Fun {Mir.funId, Mir.funCfg} = do
  mapM_ emitAsmInst (functionPrologue funId (calculateFrameSize funCfg))

  -- No need to calculate offsets - they're in the StackOperands
  translateCfg False funCfg

translateProgram' :: Mir.Program -> State TranslationState ()
translateProgram' Mir.Program {Mir.programFuns, Mir.programExternFuns, Mir.programMainFun} = do
  modify' (\s -> s {fileHeader = makeFileHeader (Mir.externId <$> programExternFuns)})

  -- translate main function
  forM_ programMainFun translateMainFun
  -- translate functions
  mapM_ translateFun programFuns

translateProgram :: Mir.Program -> String
translateProgram program = tsToAssemblyCode finalState
  where
    initialState =
      TranslationState
        { assemblyCode = [],
          lastJmpCond = Nothing,
          fileHeader = ""
        }

    (_, finalState) = runState (translateProgram' program) initialState

    tsToAssemblyCode :: TranslationState -> String
    tsToAssemblyCode TranslationState {assemblyCode, fileHeader} =
      fileHeader ++ concatMap (\inst -> show inst ++ "\n") assemblyCode

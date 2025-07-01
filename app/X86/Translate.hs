{-# OPTIONS_GHC -Wno-name-shadowing #-}

module X86.Translate where

import Control.Monad (forM_)
import Control.Monad.State (State, gets, modify', runState)
import Data.Map (Map, empty, fromList, lookup)
import Env
import Mir.Types qualified as Mir
import TypeSystem (Id, sizeOf)
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
  { varOffsets :: Map Id Int,
    assemblyCode :: [Inst],
    lastJmpCond :: Maybe JmpCond,
    fileHeader :: String
  }

instance Show TranslationState where
  show ts = ts.fileHeader ++ "\n" ++ unlines (show <$> ts.assemblyCode)

changeFlagOp :: JmpCond -> State TranslationState ()
changeFlagOp cond = modify' (\s -> s {lastJmpCond = Just cond})

emitAsmInst :: Inst -> State TranslationState ()
emitAsmInst code = modify' (\s -> s {assemblyCode = s.assemblyCode ++ [code]})

-- Offset from rbp (base pointer) for local variables
-- All variables are QWORDS and aligned
getVarOffset :: Id -> State TranslationState (Maybe Int)
getVarOffset varId = gets (Data.Map.lookup varId . (.varOffsets))

loadVarToEax :: Mir.Var -> State TranslationState ()
loadVarToEax varId = do
  case varId of
    Mir.Local {id} -> do
      maybeOffset <- getVarOffset id
      case maybeOffset of
        Just disp ->
          emitAsmInst $
            Mov
              { src = Mem {base = Rbp, disp, index_scale = Nothing},
                dst = Reg Rax
              }
        Nothing -> error $ "Variable " ++ show id ++ " not found in stack frame"
    Mir.Arg {id} -> do
      maybeOffset <- getVarOffset id
      case maybeOffset of
        Just disp ->
          emitAsmInst $
            Mov
              { src = Mem {base = Rbp, disp, index_scale = Nothing},
                dst = Reg Rax
              }
        Nothing -> error $ "Argument " ++ show id ++ " not found in stack frame"
    Mir.LocalWithOffset {id, offset, mult} -> do
      maybeOffset <- getVarOffset id
      let baseOffset = case maybeOffset of
            Just baseOffset -> baseOffset
            Nothing -> error $ "Variable " ++ show id ++ " not found in stack frame"

      case offset of
        Mir.ConstInt off ->
          emitAsmInst $
            Mov
              { src = Mem {base = Rbp, disp = baseOffset - off * mult, index_scale = Nothing},
                dst = Reg Rax
              }
        Mir.ConstChar _ ->
          error "Cannot load a character variable into rax directly, use ConstInt instead"
        Mir.Temp _ -> do
          -- pop temp into rsi
          emitAsmInst Pop {op = Reg Rsi}
          emitAsmInst $ Neg {op = Reg Rsi} -- Negate rsi to use as index

          -- then load from rsi into rax
          emitAsmInst $
            Mov
              { src = Mem {base = Rbp, disp = baseOffset, index_scale = Just (Rsi, mult)},
                dst = Reg Rax
              }

storeEaxToVar :: Mir.Var -> State TranslationState ()
storeEaxToVar varId = do
  case varId of
    Mir.Local {id} -> do
      maybeOffset <- getVarOffset id
      case maybeOffset of
        Just disp ->
          emitAsmInst $
            Mov
              { src = Reg Rax,
                dst = Mem {base = Rbp, disp, index_scale = Nothing}
              }
        Nothing -> error $ "Variable " ++ show id ++ " not found in stack frame"
    Mir.Arg {id} -> do
      maybeOffset <- getVarOffset id
      case maybeOffset of
        Just disp ->
          emitAsmInst $
            Mov
              { src = Reg Rax,
                dst = Mem {base = Rbp, disp, index_scale = Nothing}
              }
        Nothing -> error $ "Argument " ++ show id ++ " not found in stack frame"
    Mir.LocalWithOffset {id, offset, mult} -> do
      maybeOffset <- getVarOffset id
      let baseOffset = case maybeOffset of
            Just baseOffset -> baseOffset
            Nothing -> error $ "Variable " ++ show id ++ " not found in stack frame"

      case offset of
        Mir.ConstInt off -> do
          emitAsmInst $
            Mov
              { src = Reg Rax,
                dst = Mem {base = Rbp, disp = baseOffset - off * mult, index_scale = Nothing}
              }
        Mir.ConstChar _ ->
          error "Cannot store a character variable into rax directly, use ConstInt instead"
        Mir.Temp _ -> do
          -- pop temp into rsi
          emitAsmInst Pop {op = Reg Rsi}
          emitAsmInst $ Neg {op = Reg Rsi} -- Negate rsi to use as index
          -- then store from rax into rsi indexed memory location
          emitAsmInst $
            Mov
              { src = Reg Rax,
                dst = Mem {base = Rbp, disp = baseOffset, index_scale = Just (Rsi, mult)}
              }

-- All temps pass by rax register
pushTempToStack :: State TranslationState ()
pushTempToStack = emitAsmInst Push {op = Reg Rax}

popTempFromStack :: State TranslationState ()
popTempFromStack = emitAsmInst Pop {op = Reg Rax}

translateInst :: Mir.Inst -> State TranslationState ()
translateInst inst
  | Mir.Mov {srcOp} <- inst = do
      -- Translate assignment instruction
      case srcOp of
        Mir.ConstInt value -> do
          emitAsmInst Push {op = Imm value}
          return ()
        Mir.ConstChar value -> do
          emitAsmInst Push {op = Imm (fromIntegral (fromEnum value))} -- Convert char to int
          return ()
        Mir.Temp _ -> do
          -- Assuming tempOperand is already in rax
          pushTempToStack
          return ()
  | Mir.BinOp {binop, left, right} <- inst = do
      case (left, right) of
        (Mir.ConstInt leftConst, Mir.ConstInt rightConst) -> do
          emitAsmInst $ Mov {src = Imm leftConst, dst = Reg Rax}
          emitAsmInst $ Mov {src = Imm rightConst, dst = Reg Rbx}
        (Mir.Temp _, Mir.ConstInt value) -> do
          emitAsmInst $ Mov {src = Imm value, dst = Reg Rbx} -- Move constant to rbx
          emitAsmInst Pop {op = Reg Rax} -- Pop the first operand into rax
        (Mir.ConstInt value, Mir.Temp _) -> do
          emitAsmInst $ Mov {src = Imm value, dst = Reg Rax} -- Move constant to rax
          emitAsmInst Pop {op = Reg Rbx} -- Pop the second operand into rbx
        _ -> do
          emitAsmInst Pop {op = Reg Rbx} -- Pop the second operand into rbx
          emitAsmInst Pop {op = Reg Rax} -- Pop the first operand into rax
      case binop of
        TypeSystem.Add -> do
          emitAsmInst Add {src = Reg Rbx, dst = Reg Rax}
          changeFlagOp Jnz -- Add operation changes the flags
          pushTempToStack
        TypeSystem.Sub -> do
          emitAsmInst Sub {src = Reg Rbx, dst = Reg Rax}
          changeFlagOp Jnz -- Sub operation changes the flags
          pushTempToStack
        TypeSystem.Mul -> do
          emitAsmInst Imul {src = Reg Rbx, dst = Reg Rax}
          changeFlagOp Jnz -- Mul operation changes the flags
          pushTempToStack
        TypeSystem.Div -> do
          emitAsmInst Cqo -- Sign-extend rax into rdx before division
          emitAsmInst Idiv {src = Reg Rbx}
          changeFlagOp Jnz -- Div operation changes the flags
          pushTempToStack
        TypeSystem.Equal -> do
          emitAsmInst Cmp {src = Reg Rbx, dst = Reg Rax}
          changeFlagOp Je
        TypeSystem.NotEqual -> do
          emitAsmInst Cmp {src = Reg Rbx, dst = Reg Rax}
          changeFlagOp Jne
        TypeSystem.LessThan -> do
          emitAsmInst Cmp {src = Reg Rbx, dst = Reg Rax}
          changeFlagOp Jl
        TypeSystem.LessThanOrEqual -> do
          emitAsmInst Cmp {src = Reg Rbx, dst = Reg Rax}
          changeFlagOp Jle
        TypeSystem.GreaterThan -> do
          emitAsmInst Cmp {src = Reg Rbx, dst = Reg Rax}
          changeFlagOp Jg
        TypeSystem.GreaterThanOrEqual -> do
          emitAsmInst Cmp {src = Reg Rbx, dst = Reg Rax}
          changeFlagOp Jge
        TypeSystem.And -> do
          emitAsmInst And {src = Reg Rbx, dst = Reg Rax}
          changeFlagOp Jnz -- And operation changes the flags
          pushTempToStack
        TypeSystem.Or -> do
          emitAsmInst Or {src = Reg Rbx, dst = Reg Rax}
          changeFlagOp Jnz -- Or operation changes the flags
          pushTempToStack
        TypeSystem.Xor -> do
          emitAsmInst Xor {src = Reg Rbx, dst = Reg Rax}
          changeFlagOp Jnz -- Xor operation changes the flags
          pushTempToStack
        TypeSystem.Mod -> do
          emitAsmInst Cqo -- Sign-extend rax into rdx before division
          emitAsmInst Idiv {src = Reg Rbx}
          -- Result is in rdx, push it to the stack
          emitAsmInst Push {op = Reg Rdx}
          changeFlagOp Jnz -- Modulo operation changes the flags
      return ()
  | Mir.UnaryOp {unop, unsrc} <- inst = do
      case unsrc of
        Mir.ConstInt value -> do
          emitAsmInst $ Mov {src = Imm value, dst = Reg Rax} -- Move constant to rax
        Mir.ConstChar value -> do
          emitAsmInst $ Mov {src = Imm (fromIntegral (fromEnum value)), dst = Reg Rax} -- Convert char to int
        Mir.Temp _ -> do
          emitAsmInst Pop {op = Reg Rax} -- Pop the operand into rax
      case unop of
        TypeSystem.UnarySub -> do
          emitAsmInst Neg {op = Reg Rax}
          changeFlagOp Jnz -- Negation changes the flags
          pushTempToStack
        TypeSystem.UnaryNot -> do
          emitAsmInst Not {op = Reg Rax}
          changeFlagOp Jz -- Not operation changes the flags
          pushTempToStack
      return ()
  | Mir.Load {srcVar} <- inst = do
      loadVarToEax srcVar
      pushTempToStack
      return ()
  | Mir.Store {dstVar} <- inst = do
      popTempFromStack
      storeEaxToVar dstVar
      return ()
  | Mir.Call {funId, argCount, ret} <- inst = do
      emitAsmInst $ Call {name = funId}
      -- Remove arguments from the stack
      forM_ [1 .. argCount] $ \_ -> do
        emitAsmInst Pop {op = Reg Rbx} -- Pop each argument from the stack
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

translateMainFun :: Mir.Fun -> State TranslationState ()
translateMainFun Mir.Fun {locals, blocks} = do
  let (frameSize, varOffsetList) =
        foldl
          ( \(a, offsets) s -> (a + sizeOf s.ty, offsets ++ [(s.id, -a)])
          )
          (0, [])
          locals

  mapM_ emitAsmInst (mainFunctionPrologue frameSize)

  -- here 0 is the first local variable, -8 is the second, etc.
  let varOffsets = Data.Map.fromList varOffsetList
  modify' (\s -> s {varOffsets})

  mapM_ translateBasicBlock (reverse blocks)

  mapM_ emitAsmInst mainFunctionEpilogue

translateFun :: Mir.Fun -> State TranslationState ()
translateFun Mir.Fun {id, args, locals, blocks} = do
  -- Allocate space for local variables
  -- let stackFrameSize = length locals * 8 -- Assuming each argument and local variable is a QWORD (8 bytes)
  -- mapM_ emitAsmInst (functionPrologue id stackFrameSize)

  -- -- from rbp:
  -- -- at position 0 is the last rbp
  -- -- at position 8 is the return address
  -- -- at position 16 is the first argument, the second argument is at position 24, etc.
  -- let argOffsets = zip args (iterate' (+ 8) 16)

  -- -- local variables have negative disps from rbp
  -- -- at position -8 is the first local variable, at position -16 is the second local variable, etc.
  -- let varOffsetList = zip locals (iterate' (+ (-8)) (-8))

  -- let varOffsets = Data.Map.fromList $ argOffsets ++ varOffsetList
  let (frameSize, varOffsetList) =
        foldl
          ( \(a, offsets) s -> (a + sizeOf s.ty, offsets ++ [(s.id, -a)])
          )
          (0, [])
          (args ++ locals)

  mapM_ emitAsmInst (functionPrologue id frameSize)

  let (_, argOffsets) =
        foldl
          ( \(a, offsets) s -> (a + sizeOf s.ty, offsets ++ [(s.id, a)])
          )
          (16, [])
          args

  modify'
    ( \s ->
        s
          { varOffsets =
              Data.Map.fromList
                ( map (\(id, off) -> (id, off - 8)) varOffsetList
                    ++ argOffsets
                )
          }
    )

  mapM_ translateBasicBlock (reverse blocks)

  mapM_ emitAsmInst functionEpilogue

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
        { varOffsets = Data.Map.empty,
          assemblyCode = [],
          lastJmpCond = Nothing,
          fileHeader = ""
        }
    (_, finalState) = runState (translateProgram' program) initialState

module Mir.CopyPropagation
  ( eliminateRedundantAssignments,
    performCopyPropagation,
    optimizeFunction,
    optimizeProgram,
  )
where

import Data.Map (Map)
import Data.Map qualified as Map
import Mir.Types

-- Track copy relationships: temp -> operand it copies from
type CopyMap = Map Temp Operand

-- Check if an assignment is redundant (dst = dst)
isRedundantAssignment :: Inst -> Bool
isRedundantAssignment Assign {instDst = TempOperand dst, instSrc = TempOperand src} = dst == src
isRedundantAssignment _ = False

-- Check if an assignment is a simple copy (temp = temp or temp = constant)
isCopyAssignment :: Inst -> Maybe (Temp, Operand)
isCopyAssignment Assign {instDst = TempOperand dst, instSrc = src@(TempOperand _)} = Just (dst, src)
isCopyAssignment Assign {instDst = TempOperand dst, instSrc = src@(ConstInt _)} = Just (dst, src)
isCopyAssignment Assign {instDst = TempOperand dst, instSrc = src@(ConstChar _)} = Just (dst, src)
isCopyAssignment _ = Nothing

-- Get all temporaries that an instruction defines
getDefinedTemp :: Inst -> Maybe Temp
getDefinedTemp Assign {instDst = TempOperand t} = Just t
getDefinedTemp UnaryOp {instDst = TempOperand t} = Just t
getDefinedTemp BinOp {instDst = TempOperand t} = Just t
getDefinedTemp Call {callRet = Just (TempOperand t)} = Just t
getDefinedTemp _ = Nothing

-- Replace temporary uses with their copy source if beneficial
substituteOperand :: CopyMap -> Operand -> Operand
substituteOperand copyMap (TempOperand t) =
  case Map.lookup t copyMap of
    Just replacement -> replacement
    Nothing -> TempOperand t
substituteOperand _ op = op

-- Apply copy propagation to an instruction
propagateInInstruction :: CopyMap -> Inst -> Inst
propagateInInstruction copyMap inst = case inst of
  Assign {instDst, instSrc} ->
    Assign {instDst = instDst, instSrc = substituteOperand copyMap instSrc}
  UnaryOp {instDst, instUnop, instSrc} ->
    UnaryOp {instDst = instDst, instUnop = instUnop, instSrc = substituteOperand copyMap instSrc}
  BinOp {instDst, instBinop, instLeft, instRight} ->
    BinOp
      { instDst = instDst,
        instBinop = instBinop,
        instLeft = substituteOperand copyMap instLeft,
        instRight = substituteOperand copyMap instRight
      }
  Param {paramOperand} ->
    Param {paramOperand = substituteOperand copyMap paramOperand}
  Call {} -> inst -- Calls don't use operands directly

-- Update copy map after processing an instruction
updateCopyMap :: CopyMap -> Inst -> CopyMap
updateCopyMap copyMap inst
  | Just (dst, src) <- isCopyAssignment inst = Map.insert dst src copyMap
  | Just dst <- getDefinedTemp inst = Map.delete dst copyMap -- Kill existing copy
  | otherwise = copyMap

-- Perform copy propagation on a basic block
propagateInBlock :: BasicBlock -> BasicBlock
propagateInBlock (BasicBlock blockId insts terminator) =
  let (newInsts, _) = foldl processInst ([], Map.empty) insts
      newTerminator = propagateInTerminator (snd $ foldl processInst ([], Map.empty) insts) terminator
   in BasicBlock blockId (reverse newInsts) newTerminator
  where
    processInst :: ([Inst], CopyMap) -> Inst -> ([Inst], CopyMap)
    processInst (acc, copyMap) inst =
      let propagatedInst = propagateInInstruction copyMap inst
          newCopyMap = updateCopyMap copyMap propagatedInst
       in (propagatedInst : acc, newCopyMap)

-- Apply copy propagation to terminators
propagateInTerminator :: CopyMap -> Terminator -> Terminator
propagateInTerminator copyMap term = case term of
  Return {retOperand = Just op} -> Return {retOperand = Just (substituteOperand copyMap op)}
  CondJump {condOperand, condTrueBasicBlockId, condFalseBasicBlockId} ->
    CondJump
      { condOperand = substituteOperand copyMap condOperand,
        condTrueBasicBlockId = condTrueBasicBlockId,
        condFalseBasicBlockId = condFalseBasicBlockId
      }
  _ -> term

-- Remove redundant assignments from a list of instructions
eliminateRedundantAssignments :: [Inst] -> [Inst]
eliminateRedundantAssignments = filter (not . isRedundantAssignment)

-- Perform copy propagation on entire CFG
performCopyPropagation :: CFG -> CFG
performCopyPropagation cfg =
  cfg {cfgBlocks = map propagateInBlock (cfgBlocks cfg)}

-- Remove redundant assignments from CFG
eliminateRedundantInCFG :: CFG -> CFG
eliminateRedundantInCFG cfg =
  cfg {cfgBlocks = map eliminateInBlock (cfgBlocks cfg)}
  where
    eliminateInBlock (BasicBlock blockId insts term) =
      BasicBlock blockId (eliminateRedundantAssignments insts) term

-- Combined optimization: copy propagation + redundant elimination
optimizeFunction :: Fun -> Fun
optimizeFunction fun@Fun {funCfg} =
  let step1 = performCopyPropagation funCfg
      step2 = eliminateRedundantInCFG step1
   in fun {funCfg = step2}

-- Iterative optimization until fixed point
iterativeOptimization :: Fun -> Fun
iterativeOptimization fun =
  let optimized = optimizeFunction fun
   in if funCfg optimized == funCfg fun
        then optimized
        else iterativeOptimization optimized

-- Optimize entire program
optimizeProgram :: Program -> Program
optimizeProgram program@Program {programFuns, programMainFun} =
  program
    { programFuns = map iterativeOptimization programFuns,
      programMainFun = fmap iterativeOptimization programMainFun
    }
module Mir.Allocation
  ( AllocationResult (..),
    SpillLocation (..),
    RegisterAssignment,
    allocateRegisters,
    applyAllocation,
    allocateFunction,
    allocateProgram,
  )
where

import Data.List (sortOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Mir.Interference
import Mir.Liveness
import Mir.Types

-- Register assignment: temp -> register
type RegisterAssignment = Map Temp Register

-- Spill handling - convert temps to memory locations
newtype SpillLocation = StackSlot Int deriving (Eq, Ord, Show)

data AllocationResult = AllocationResult
  { allocRegisters :: RegisterAssignment,
    allocSpilled :: Map Temp SpillLocation
  }
  deriving (Show)

-- Simple greedy graph coloring
colorGraph :: InterferenceGraph -> Maybe RegisterAssignment
colorGraph g@(InterferenceGraph graph) = colorTemps g sortedTemps Map.empty
  where
    temps = Map.keys graph
    sortedTemps = sortByDegree g temps

-- Sort temporaries by interference degree (most constrained first)
sortByDegree :: InterferenceGraph -> [Temp] -> [Temp]
sortByDegree (InterferenceGraph graph) = sortOn (negate . getDegree)
  where
    getDegree t = Set.size $ Map.findWithDefault Set.empty t graph

-- Color temporaries one by one
colorTemps :: InterferenceGraph -> [Temp] -> RegisterAssignment -> Maybe RegisterAssignment
colorTemps _graph [] assignment = Just assignment
colorTemps graph (temp : rest) assignment =
  case findAvailableRegister graph temp assignment of
    Nothing -> Nothing -- Spilling needed
    Just reg -> colorTemps graph rest (Map.insert temp reg assignment)

-- Find an available register for a temporary
findAvailableRegister :: InterferenceGraph -> Temp -> RegisterAssignment -> Maybe Register
findAvailableRegister (InterferenceGraph graph) temp assignment =
  case availableRegs of
    (reg : _) -> Just reg
    [] -> Nothing
  where
    interferers = Map.findWithDefault Set.empty temp graph
    usedRegs =
      Set.fromList
        [ reg | t <- Set.toList interferers, Just reg <- [Map.lookup t assignment]
        ]
    availableRegs = filter (`Set.notMember` usedRegs) availableRegisters

-- Allocate with spilling support
allocateRegisters :: CFG -> LivenessInfo -> AllocationResult
allocateRegisters cfg liveness =
  let interferenceGraph = buildInterferenceGraph cfg liveness
   in case colorGraph interferenceGraph of
        Just assignment -> AllocationResult {allocRegisters = assignment, allocSpilled = Map.empty}
        Nothing -> allocateWithSpilling cfg liveness interferenceGraph

-- Simple spilling strategy - spill highest degree nodes
allocateWithSpilling :: CFG -> LivenessInfo -> InterferenceGraph -> AllocationResult
allocateWithSpilling cfg _liveness graph =
  let allTemps = Set.toList $ getAllTemps cfg
      (spillCandidates, _colorableTemps) = selectSpillCandidates graph allTemps
      spillAssignment = Map.fromList $ zip spillCandidates (map StackSlot [0 ..])
      reducedGraph = reAssigneSpilledFromGraph spillCandidates graph
   in case colorGraph reducedGraph of
        Just regAssignment -> AllocationResult {allocRegisters = regAssignment, allocSpilled = spillAssignment}
        Nothing -> error "Still can't color after spilling" -- Need more sophisticated spilling

-- Select temps to spill (simple heuristic: highest degree)
selectSpillCandidates :: InterferenceGraph -> [Temp] -> ([Temp], [Temp])
selectSpillCandidates graph temps =
  let sortedByDegree = sortByDegree graph temps
      numToSpill = max 0 (length temps - length availableRegisters)
      (toSpill, toColor) = splitAt numToSpill sortedByDegree
   in (toSpill, toColor)

-- ReAssigne spilled temporaries from interference graph
reAssigneSpilledFromGraph :: [Temp] -> InterferenceGraph -> InterferenceGraph
reAssigneSpilledFromGraph spilled (InterferenceGraph graph) =
  let spilledSet = Set.fromList spilled
   in InterferenceGraph $
        Map.map (Set.filter (`Set.notMember` spilledSet)) $
          Map.filterWithKey (\k _ -> k `Set.notMember` spilledSet) graph

-- Apply register allocation to transform MIR
applyAllocation :: AllocationResult -> Fun -> Fun
applyAllocation allocation fun =
  fun {funCfg = transformCFG allocation (funCfg fun)}

transformCFG :: AllocationResult -> CFG -> CFG
transformCFG allocation cfg =
  cfg {cfgBlocks = map (transformBlock allocation) (cfgBlocks cfg)}

transformBlock :: AllocationResult -> BasicBlock -> BasicBlock
transformBlock allocation block =
  block
    { blockInsts = map (transformInst allocation) (blockInsts block),
      blockTerminator = transformTerminator allocation (blockTerminator block)
    }

transformInst :: AllocationResult -> Inst -> Inst
transformInst allocation inst = case inst of
  Assign {instDst, instSrc} ->
    Assign
      { instDst = transformOperand allocation instDst,
        instSrc = transformOperand allocation instSrc
      }
  UnaryOp {instDst, instUnop, instSrc} ->
    UnaryOp
      { instDst = transformOperand allocation instDst,
        instUnop = instUnop,
        instSrc = transformOperand allocation instSrc
      }
  BinOp {instDst, instBinop, instLeft, instRight} ->
    BinOp
      { instDst = transformOperand allocation instDst,
        instBinop = instBinop,
        instLeft = transformOperand allocation instLeft,
        instRight = transformOperand allocation instRight
      }
  Call {callRet, callFunId, callArgCount} ->
    Call
      { callRet = fmap (transformOperand allocation) callRet,
        callFunId = callFunId,
        callArgCount = callArgCount
      }
  Param {paramOperand} ->
    Param
      { paramOperand = transformOperand allocation paramOperand
      }

transformTerminator :: AllocationResult -> Terminator -> Terminator
transformTerminator _allocation terminator = case terminator of
  Return {retOperand} -> Return {retOperand = fmap (transformOperand _allocation) retOperand}
  Jump {jumpTarget} -> Jump {jumpTarget = jumpTarget} -- No transformation needed for Jump
  CondJump {condOperand, condTrueBasicBlockId, condFalseBasicBlockId} ->
    CondJump
      { condOperand = transformOperand _allocation condOperand,
        condTrueBasicBlockId = condTrueBasicBlockId,
        condFalseBasicBlockId = condFalseBasicBlockId
      }

transformOperand :: AllocationResult -> Operand -> Operand
transformOperand allocation (TempOperand t) =
  case Map.lookup t (allocRegisters allocation) of
    Just reg -> RegOperand reg
    Nothing -> case Map.lookup t (allocSpilled allocation) of
      Just (StackSlot slot) -> StackOperand slot
      Nothing -> error $ "Unallocated temporary: " ++ show t
transformOperand _ op = op

allocateFunction :: Fun -> Fun
allocateFunction fun =
  let liveness = analyzeFunctionLiveness fun
      allocation = allocateRegisters (funCfg fun) liveness
   in applyAllocation allocation fun

allocateProgram :: Program -> Program
allocateProgram p@Program {programFuns, programMainFun = Just mainFun} =
  let allocatedFuns = map allocateFunction programFuns
      allocatedMain = allocateFunction mainFun
   in p {programFuns = allocatedFuns, programMainFun = Just allocatedMain}
allocateProgram p@Program {programFuns, programMainFun = Nothing} =
  let allocatedFuns = map allocateFunction programFuns
   in p {programFuns = allocatedFuns, programMainFun = Nothing}
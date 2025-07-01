{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Mir.Allocation where

import Data.List (sortOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Mir.Interference
import Mir.Liveness
import Mir.Types

-- Register assignment: temp -> register
type RegisterAssignment = Map Temp Register

-- Simple greedy graph coloring
colorGraph :: InterferenceGraph -> Maybe RegisterAssignment
colorGraph graph =
  let temps = Map.keys graph
      sortedTemps = sortByDegree graph temps
   in colorTemps graph sortedTemps Map.empty

-- Sort temporaries by interference degree (most constrained first)
sortByDegree :: InterferenceGraph -> [Temp] -> [Temp]
sortByDegree graph temps =
  let getDegree t = Set.size $ Map.findWithDefault Set.empty t graph
   in sortOn (negate . getDegree) temps

-- Color temporaries one by one
colorTemps :: InterferenceGraph -> [Temp] -> RegisterAssignment -> Maybe RegisterAssignment
colorTemps _graph [] assignment = Just assignment
colorTemps graph (temp : rest) assignment =
  case findAvailableRegister graph temp assignment of
    Nothing -> Nothing -- Spilling needed
    Just reg -> colorTemps graph rest (Map.insert temp reg assignment)

-- Find an available register for a temporary
findAvailableRegister :: InterferenceGraph -> Temp -> RegisterAssignment -> Maybe Register
findAvailableRegister graph temp assignment =
  let interferers = Map.findWithDefault Set.empty temp graph
      usedRegs =
        Set.fromList
          [ reg | t <- Set.toList interferers, Just reg <- [Map.lookup t assignment]
          ]
      availableRegs = filter (`Set.notMember` usedRegs) availableRegisters
   in case availableRegs of
        (reg : _) -> Just reg
        [] -> Nothing

-- Spill handling - convert temps to memory locations
newtype SpillLocation = StackSlot Int deriving (Eq, Ord, Show)

data AllocationResult = AllocationResult
  { registers :: RegisterAssignment,
    spilled :: Map Temp SpillLocation
  }
  deriving (Show)

-- Allocate with spilling support
allocateRegisters :: CFG -> LivenessInfo -> AllocationResult
allocateRegisters cfg liveness =
  let interferenceGraph = buildInterferenceGraph cfg liveness
   in case colorGraph interferenceGraph of
        Just assignment -> AllocationResult {registers = assignment, spilled = Map.empty}
        Nothing -> allocateWithSpilling cfg liveness interferenceGraph

-- Simple spilling strategy - spill highest degree nodes
allocateWithSpilling :: CFG -> LivenessInfo -> InterferenceGraph -> AllocationResult
allocateWithSpilling cfg _liveness graph =
  let allTemps = Set.toList $ getAllTemps cfg
      (spillCandidates, _colorableTemps) = selectSpillCandidates graph allTemps
      spillAssignment = Map.fromList $ zip spillCandidates (map StackSlot [0 ..])
      reducedGraph = reAssigneSpilledFromGraph spillCandidates graph
   in case colorGraph reducedGraph of
        Just regAssignment -> AllocationResult {registers = regAssignment, spilled = spillAssignment}
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
reAssigneSpilledFromGraph spilled graph =
  let spilledSet = Set.fromList spilled
   in Map.map (Set.filter (`Set.notMember` spilledSet)) $
        Map.filterWithKey (\k _ -> k `Set.notMember` spilledSet) graph

-- Apply register allocation to transform MIR
applyAllocation :: AllocationResult -> Fun -> Fun
applyAllocation allocation fun =
  fun {cfg = transformCFG allocation fun.cfg}

transformCFG :: AllocationResult -> CFG -> CFG
transformCFG allocation cfg =
  cfg {blocks = map (transformBlock allocation) cfg.blocks}

transformBlock :: AllocationResult -> BasicBlock -> BasicBlock
transformBlock allocation block =
  block
    { insts = map (transformInst allocation) block.insts,
      terminator = transformTerminator allocation block.terminator
    }

transformInst :: AllocationResult -> Inst -> Inst
transformInst allocation inst = case inst of
  Assign {dst, src} ->
    Assign
      { dst = transformOperand allocation dst, -- Transform destination!
        src = transformOperand allocation src
      }
  UnaryOp {dst, unop, src} ->
    UnaryOp
      { dst = transformOperand allocation dst, -- Transform destination!
        unop = unop,
        src = transformOperand allocation src
      }
  BinOp {dst, binop, left, right} ->
    BinOp
      { dst = transformOperand allocation dst, -- Transform destination!
        binop = binop,
        left = transformOperand allocation left,
        right = transformOperand allocation right
      }
  Load {dst, srcVar} ->
    Load
      { dst = transformOperand allocation dst, -- Transform destination!
        srcVar = transformVar allocation srcVar -- Also transform variables with temp offsets
      }
  Store {dstVar, src} ->
    Store
      { dstVar = transformVar allocation dstVar, -- Transform variables with temp offsets
        src = src -- This is a bare Temp, needs separate handling
      }
  Call {ret, funId, argCount} ->
    Call
      { ret = fmap (transformOperand allocation) ret, -- Transform return operand
        funId = funId,
        argCount = argCount
      }
  Param {param} ->
    Param {param = param} -- This is a bare Temp, needs separate handling

-- You'll also need this helper for Var transformations
transformVar :: AllocationResult -> Var -> Var
transformVar allocation var = case var of
  Local {id} -> Local {id}
  Arg {id} -> Arg {id}
  LocalWithOffset {id, offset, mult} ->
    LocalWithOffset
      { id = id,
        offset = transformOperand allocation offset, -- Transform offset operand
        mult = mult
      }

-- And for bare Temp fields, you might want a separate function
transformTemp :: AllocationResult -> Temp -> Temp
transformTemp allocation temp =
  case transformOperand allocation (Temp temp) of
    Temp t -> t -- If it stayed a temp (spilled)
    RegOperand _ -> error "Cannot convert register to temp directly"
    StackOperand _ -> error "Cannot convert stack slot to temp directly"
    _ -> error "Unexpected operand type"

transformTerminator :: AllocationResult -> Terminator -> Terminator
transformTerminator _allocation terminator = case terminator of
  Return {retVal} -> Return {retVal = retVal} -- Return values are temps, not operands
  Jump {target} -> Jump {target = target}
  CondJump {cond, trueBlockId, falseBlockId} ->
    CondJump {cond = cond, trueBlockId = trueBlockId, falseBlockId = falseBlockId}

-- Transform operands based on allocation
transformOperand :: AllocationResult -> Operand -> Operand
transformOperand allocation (Temp t) =
  case Map.lookup t allocation.registers of
    Just reg -> RegOperand reg -- Need to add this to Operand type
    Nothing -> case Map.lookup t allocation.spilled of
      Just (StackSlot slot) -> StackOperand slot -- Need to add this too
      Nothing -> error $ "Unallocated temporary: " ++ show t
transformOperand _ op = op

allocateFunction :: Fun -> Fun
allocateFunction fun =
  let liveness = analyzeFunctionLiveness fun
      allocation = allocateRegisters fun.cfg liveness
   in applyAllocation allocation fun

allocateProgram :: Program -> Program
allocateProgram p@Program {funs, mainFun = Just mainFun} =
  let allocatedFuns = map allocateFunction funs
      allocatedMain = allocateFunction mainFun
   in p {funs = allocatedFuns, mainFun = Just allocatedMain}
allocateProgram p@Program {funs, mainFun = Nothing} =
  let allocatedFuns = map allocateFunction funs
   in p {funs = allocatedFuns, mainFun = Nothing}
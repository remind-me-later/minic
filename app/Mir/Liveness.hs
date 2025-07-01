{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Mir.Liveness where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Mir.Types
import TypeSystem (Id)

-- Add after existing types
type LiveSet = Set Temp

data LivenessInfo = LivenessInfo
  { livenessIn :: Map BlockId LiveSet,
    livenessOut :: Map BlockId LiveSet
  }
  deriving (Eq)

instance Show LivenessInfo where
  show (LivenessInfo liveIn liveOut) =
    "LivenessInfo:\n"
      ++ unlines [showBlock bid | bid <- Map.keys liveIn]
    where
      showBlock bid =
        let inSet = Map.findWithDefault Set.empty bid liveIn
            outSet = Map.findWithDefault Set.empty bid liveOut
         in "Block "
              ++ bid
              ++ ": IN = {"
              ++ unwords (map (("t" ++) . show) (Set.toList inSet))
              ++ "}, OUT = {"
              ++ unwords (map (("t" ++) . show) (Set.toList outSet))
              ++ "}"

-- Add these functions for live variable analysis

-- Get all temporaries used (read) by an instruction
getUsedTemps :: Inst -> Set Temp
getUsedTemps inst = case inst of
  Assign {instSrc} -> getOperandTemps instSrc
  UnaryOp {instSrc} -> getOperandTemps instSrc
  BinOp {instLeft, instRight} -> getOperandTemps instLeft <> getOperandTemps instRight
  Call {} -> Set.empty -- args passed via Param instructions
  Param {paramOperand} -> getOperandTemps paramOperand

-- Get temporaries defined (written) by an instruction
getDefinedTemps :: Inst -> Set Temp
getDefinedTemps inst = case inst of
  Assign {instDst} -> getOperandTemps instDst
  UnaryOp {instDst} -> getOperandTemps instDst
  BinOp {instDst} -> getOperandTemps instDst
  Call {callRet = Just (Temp ret)} -> Set.singleton ret
  _ -> Set.empty

-- Get temporaries from operands
getOperandTemps :: Operand -> Set Temp
getOperandTemps (Temp t) = Set.singleton t
getOperandTemps _ = Set.empty

-- Get temporaries used by terminators
getTerminatorUses :: Terminator -> Set Temp
getTerminatorUses Return {retOperand = Just t} = getOperandTemps t
getTerminatorUses Return {retOperand = Nothing} = Set.empty
getTerminatorUses Jump {} = Set.empty
getTerminatorUses CondJump {condOperand} = getOperandTemps condOperand

-- Get successor blocks from terminator
getSuccessors :: Terminator -> [BlockId]
getSuccessors Return {} = []
getSuccessors Jump {jumpTarget} = [jumpTarget]
getSuccessors CondJump {condTrueBlockId, condFalseBlockId} = [condTrueBlockId, condFalseBlockId]

-- Build predecessor map for CFG
buildPredecessorMap :: CFG -> Map BlockId [BlockId]
buildPredecessorMap cfg =
  let allBlocks = cfgBlocks cfg
      addEdges block acc =
        let successors = getSuccessors (blockTerminator block)
            blockId' = cfgBlockId block
         in foldr (\succ -> Map.insertWith (++) succ [blockId']) acc successors
   in foldr addEdges Map.empty allBlocks

-- Perform live variable analysis on a CFG
performLivenessAnalysis :: CFG -> LivenessInfo
performLivenessAnalysis cfg =
  let blockList = cfgBlocks cfg
      blockMap = Map.fromList [(cfgBlockId b, b) | b <- blockList]
      predecessors = buildPredecessorMap cfg

      -- Initialize all sets to empty
      initialLiveIn = Map.fromList [(cfgBlockId b, Set.empty) | b <- blockList]
      initialLiveOut = Map.fromList [(cfgBlockId b, Set.empty) | b <- blockList]
   in fixedPoint blockMap predecessors initialLiveIn initialLiveOut

-- Fixed-point iteration for liveness
fixedPoint ::
  Map BlockId BasicBlock ->
  Map BlockId [BlockId] ->
  Map BlockId LiveSet ->
  Map BlockId LiveSet ->
  LivenessInfo
fixedPoint blockMap predecessors liveIn liveOut =
  let (newLiveIn, newLiveOut) = oneIteration blockMap predecessors liveIn liveOut
   in if newLiveIn == liveIn && newLiveOut == liveOut
        then LivenessInfo {livenessIn = newLiveIn, livenessOut = newLiveOut}
        else fixedPoint blockMap predecessors newLiveIn newLiveOut

-- One iteration of the liveness algorithm
oneIteration ::
  Map BlockId BasicBlock ->
  Map BlockId [BlockId] ->
  Map BlockId LiveSet ->
  Map BlockId LiveSet ->
  (Map BlockId LiveSet, Map BlockId LiveSet)
oneIteration blockMap predecessors oldLiveIn oldLiveOut =
  let blockIds = Map.keys blockMap
      (newLiveIn, newLiveOut) =
        foldr
          (updateBlock blockMap predecessors oldLiveOut)
          (oldLiveIn, oldLiveOut)
          blockIds
   in (newLiveIn, newLiveOut)

-- Update liveness for a single block
updateBlock ::
  Map BlockId BasicBlock ->
  Map BlockId [BlockId] ->
  Map BlockId LiveSet ->
  BlockId ->
  (Map BlockId LiveSet, Map BlockId LiveSet) ->
  (Map BlockId LiveSet, Map BlockId LiveSet)
updateBlock blockMap _predecessors _oldLiveOut blockId (liveIn, liveOut) =
  case Map.lookup blockId blockMap of
    Nothing -> (liveIn, liveOut)
    Just block ->
      let -- OUT[B] = union of IN[S] for all successors S of B
          successors = getSuccessors (blockTerminator block)
          newOut = Set.unions [Map.findWithDefault Set.empty s liveIn | s <- successors]

          -- Calculate used and defined temps for this block
          (used, defined) = analyzeBlock block

          -- IN[B] = USE[B] ∪ (OUT[B] - DEF[B])
          newIn = used `Set.union` (newOut `Set.difference` defined)
       in (Map.insert blockId newIn liveIn, Map.insert blockId newOut liveOut)

-- Analyze a block to get used and defined temporaries
analyzeBlock :: BasicBlock -> (Set Temp, Set Temp)
analyzeBlock (BasicBlock _ insts term) =
  let -- Process instructions in reverse order for accurate liveness
      (used, defined) = foldr processInstruction (Set.empty, Set.empty) insts
      termUses = getTerminatorUses term
      finalUsed = used `Set.union` termUses
   in (finalUsed, defined)

-- Process a single instruction for liveness
processInstruction :: Inst -> (Set Temp, Set Temp) -> (Set Temp, Set Temp)
processInstruction inst (used, defined) =
  let instUses = getUsedTemps inst
      instDefs = getDefinedTemps inst
      -- ReAssigne newly defined temps from used set, add new uses
      newUsed = (used `Set.difference` instDefs) `Set.union` instUses
      newDefined = defined `Set.union` instDefs
   in (newUsed, newDefined)

analyzeFunctionLiveness :: Fun -> LivenessInfo
analyzeFunctionLiveness Fun {funCfg} = performLivenessAnalysis funCfg

analyzeProgramLiveness :: Program -> Map Id LivenessInfo
analyzeProgramLiveness Program {programFuns, programMainFun} =
  Map.fromList [(funId fun, analyzeFunctionLiveness fun) | fun <- programFuns ++ maybeToList programMainFun]
  where
    maybeToList Nothing = []
    maybeToList (Just x) = [x]
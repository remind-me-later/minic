{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Mir.Liveness
  ( LivenessInfo (..),
    LiveSet,
    analyzeFunctionLiveness,
    analyzeProgramLiveness,
  )
where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Mir.Types
import TypeSystem (Id)

-- Add after existing types
type LiveSet = Set Temp

data LivenessInfo = LivenessInfo
  { liveIn :: Map BlockId LiveSet,
    liveOut :: Map BlockId LiveSet
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
  Mov {srcOp} -> getOperandTemps srcOp
  UnaryOp {unsrc = srcOp} -> getOperandTemps srcOp
  BinOp {left, right} -> getOperandTemps left <> getOperandTemps right
  Load {} -> Set.empty -- variables, not temps
  Store {src} -> Set.singleton src
  Call {} -> Set.empty -- args passed via Param instructions
  Param {param} -> Set.singleton param

-- Get temporaries defined (written) by an instruction
getDefinedTemps :: Inst -> Set Temp
getDefinedTemps inst = case inst of
  Mov dst _ -> Set.singleton dst
  UnaryOp dst _ _ -> Set.singleton dst
  BinOp dst _ _ _ -> Set.singleton dst
  Load dst _ -> Set.singleton dst
  Store _ _ -> Set.empty
  Call (Just ret) _ _ -> Set.singleton ret
  Call Nothing _ _ -> Set.empty
  Param _ -> Set.empty

-- Get temporaries from operands
getOperandTemps :: Operand -> Set Temp
getOperandTemps (Temp t) = Set.singleton t
getOperandTemps _ = Set.empty

-- Get temporaries used by terminators
getTerminatorUses :: Terminator -> Set Temp
getTerminatorUses (Return (Just t)) = Set.singleton t
getTerminatorUses (Return Nothing) = Set.empty
getTerminatorUses (Jump _) = Set.empty
getTerminatorUses (CondJump cond _ _) = Set.singleton cond

-- Get successor blocks from terminator
getSuccessors :: Terminator -> [BlockId]
getSuccessors (Return _) = []
getSuccessors (Jump target) = [target]
getSuccessors (CondJump _ trueId falseId) = [trueId, falseId]

-- Build predecessor map for CFG
buildPredecessorMap :: CFG -> Map BlockId [BlockId]
buildPredecessorMap cfg =
  let allBlocks = cfg.blocks
      addEdges block acc =
        let successors = getSuccessors block.terminator
            blockId' = block.blockId
         in foldr (\succ -> Map.insertWith (++) succ [blockId']) acc successors
   in foldr addEdges Map.empty allBlocks

-- Perform live variable analysis on a CFG
performLivenessAnalysis :: CFG -> LivenessInfo
performLivenessAnalysis cfg =
  let blockList = cfg.blocks
      blockMap = Map.fromList [(b.blockId, b) | b <- blockList]
      predecessors = buildPredecessorMap cfg

      -- Initialize all sets to empty
      initialLiveIn = Map.fromList [(b.blockId, Set.empty) | b <- blockList]
      initialLiveOut = Map.fromList [(b.blockId, Set.empty) | b <- blockList]
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
        then LivenessInfo {liveIn = newLiveIn, liveOut = newLiveOut}
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
          successors = getSuccessors block.terminator
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
      -- Remove newly defined temps from used set, add new uses
      newUsed = (used `Set.difference` instDefs) `Set.union` instUses
      newDefined = defined `Set.union` instDefs
   in (newUsed, newDefined)

analyzeFunctionLiveness :: Fun -> LivenessInfo
analyzeFunctionLiveness Fun {cfg} = performLivenessAnalysis cfg

analyzeProgramLiveness :: Program -> Map Id LivenessInfo
analyzeProgramLiveness Program {funs, mainFun} =
  Map.fromList [(fun.id, analyzeFunctionLiveness fun) | fun <- funs ++ maybeToList mainFun]
  where
    maybeToList Nothing = []
    maybeToList (Just x) = [x]
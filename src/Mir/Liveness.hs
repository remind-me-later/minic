-- Dragon book: 9.2.5 Live-Variable Analysis
{-# LANGUAGE TypeFamilies #-}

module Mir.Liveness
  ( LivenessInfo (..),
    analyzeFunctionLiveness,
    analyzeProgramLiveness,
    getUsedTemps,
    getDefinedTemps,
    getTerminatorUses,
    performLivenessAnalysis,
    getLiveAtInstruction,
  )
where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Mir.Types
import TypeSystem (Id)

type LiveSet = Set Temp

data LivenessInfo = LivenessInfo
  { livenessIn :: Map BasicBlockId LiveSet,
    livenessOut :: Map BasicBlockId LiveSet
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
              ++ unwords (map show (Set.toList inSet))
              ++ "}, OUT = {"
              ++ unwords (map show (Set.toList outSet))
              ++ "}"

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
  _ -> Set.empty

getOperandTemps :: Operand -> Set Temp
getOperandTemps (TempOperand t) = Set.singleton t
getOperandTemps _ = Set.empty

getTerminatorUses :: Terminator -> Set Temp
getTerminatorUses CondJump {condOperand} = getOperandTemps condOperand
getTerminatorUses Return {retOperand} = maybe Set.empty getOperandTemps retOperand
getTerminatorUses _ = Set.empty

-- Get successor blocks from terminator
getSuccessors :: Terminator -> [BasicBlockId]
getSuccessors Return {} = []
getSuccessors Jump {jumpTarget} = [jumpTarget]
getSuccessors CondJump {condTrueBasicBlockId, condFalseBasicBlockId} = [condTrueBasicBlockId, condFalseBasicBlockId]

-- Algebra for fixed point liveness analysis
data LivenessState = LivenessState
  { lsLiveIn :: Map BasicBlockId LiveSet,
    lsLiveOut :: Map BasicBlockId LiveSet,
    lsPredecessors :: Map BasicBlockId [BasicBlockId],
    lsConverged :: Bool
  }
  deriving (Eq)

-- Helper to build predecessor map
buildPredecessorMap :: CFG -> Map BasicBlockId [BasicBlockId]
buildPredecessorMap CFG {cfgBlocks} = foldr addEdges Map.empty cfgBlocks
  where
    addEdges BasicBlock {blockTerminator, cfgBasicBlockId} acc =
      foldr (\nextBlock -> Map.insertWith (++) nextBlock [cfgBasicBlockId]) acc successors
      where
        successors = getSuccessors blockTerminator

-- Simplified approach without CFG recursion schemes
performLivenessAnalysis :: CFG -> LivenessInfo
performLivenessAnalysis cfg =
  let initialState = LivenessState initialLiveIn initialLiveOut predecessors False
      finalState = iterateUntilConverged cfg initialState
   in LivenessInfo (lsLiveIn finalState) (lsLiveOut finalState)
  where
    blockList = cfgBlocks cfg
    predecessors = buildPredecessorMap cfg
    initialLiveIn = Map.fromList [(cfgBasicBlockId b, Set.empty) | b <- blockList]
    initialLiveOut = Map.fromList [(cfgBasicBlockId b, Set.empty) | b <- blockList]

-- Simple iteration without recursion schemes
iterateUntilConverged :: CFG -> LivenessState -> LivenessState
iterateUntilConverged cfg state
  | lsConverged state = state
  | otherwise = iterateUntilConverged cfg (oneIteration blockMap state)
  where
    blockMap = Map.fromList [(cfgBasicBlockId b, b) | b <- cfgBlocks cfg]

-- One iteration of the liveness analysis
oneIteration :: Map BasicBlockId BasicBlock -> LivenessState -> LivenessState
oneIteration blockMap oldState =
  LivenessState newLiveIn newLiveOut (lsPredecessors oldState) converged
  where
    blockIds = Map.keys blockMap
    (newLiveIn, newLiveOut) =
      foldr
        (updateBlockLiveness blockMap oldState)
        (lsLiveIn oldState, lsLiveOut oldState)
        blockIds
    converged = newLiveIn == lsLiveIn oldState && newLiveOut == lsLiveOut oldState

updateBlockLiveness ::
  Map BasicBlockId BasicBlock ->
  LivenessState ->
  BasicBlockId ->
  (Map BasicBlockId LiveSet, Map BasicBlockId LiveSet) ->
  (Map BasicBlockId LiveSet, Map BasicBlockId LiveSet)
updateBlockLiveness blockMap state blockId (liveIn, liveOut) =
  case Map.lookup blockId blockMap of
    Nothing -> (liveIn, liveOut)
    Just block -> (Map.insert blockId newIn liveIn, Map.insert blockId newOut liveOut)
      where
        -- OUT[B] = union of IN[S] for all successors S of B
        successors = getSuccessors (blockTerminator block)
        newOut = Set.unions [Map.findWithDefault Set.empty s (lsLiveIn state) | s <- successors]

        -- Calculate used and defined temps for this block using recursion schemes
        (used, defined) = analyzeBlockLiveness block

        -- IN[B] = USE[B] ∪ (OUT[B] - DEF[B])
        isEntryBlock = null (Map.findWithDefault [] blockId (lsPredecessors state))
        newIn =
          if isEntryBlock
            then Set.empty
            else used `Set.union` (newOut `Set.difference` defined)

        -- Analyze a block to get variables used before
        -- and variables defined in it (defined)
        -- This is done in reverse order to ensure liveness is calculated correctly
        analyzeBlockLiveness :: BasicBlock -> (Set Temp, Set Temp)
        analyzeBlockLiveness (BasicBlock _ insts term) = (used', defined')
          where
            termUses = getTerminatorUses term
            (used', defined') = foldr processInstruction (termUses, Set.empty) insts

            processInstruction :: Inst -> (Set Temp, Set Temp) -> (Set Temp, Set Temp)
            processInstruction inst (used'', defined'') = (newUsed, newDefined)
              where
                -- `used` are the temps used before definition until now in the block (reverse order)
                -- `defined` are the temps defined in the block
                instUses = getUsedTemps inst
                instDefs = getDefinedTemps inst
                -- ReAssigne newly defined temps from used set, add new uses
                newUsed = (used'' `Set.difference` instDefs) `Set.union` instUses
                newDefined = defined'' `Set.union` instDefs

getLiveAtInstruction :: LivenessInfo -> BasicBlockId -> Inst -> Set Temp
getLiveAtInstruction liveness blockId inst =
  let liveOut = Map.findWithDefault Set.empty blockId (livenessOut liveness)
      defined = getDefinedTemps inst
      used = getUsedTemps inst
   in (liveOut `Set.difference` defined) `Set.union` used

analyzeFunctionLiveness :: Fun -> LivenessInfo
analyzeFunctionLiveness Fun {funCfg} = performLivenessAnalysis funCfg

analyzeProgramLiveness :: Program -> Map Id LivenessInfo
analyzeProgramLiveness Program {programFuns, programMainFun} =
  Map.fromList [(funId fun, analyzeFunctionLiveness fun) | fun <- programFuns ++ maybeToList programMainFun]
  where
    maybeToList Nothing = []
    maybeToList (Just x) = [x]
-- Dragon book: 9.2.5 Live-Variable Analysis
{-# OPTIONS_GHC -Wno-name-shadowing #-}

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
getOperandTemps (Temp t) = Set.singleton t
getOperandTemps _ = Set.empty

getTerminatorUses :: Terminator -> Set Temp
getTerminatorUses CondJump {condOperand} = getOperandTemps condOperand
getTerminatorUses Return {retOperand} = maybe Set.empty getOperandTemps retOperand
getTerminatorUses _ = Set.empty

-- Get successor blocks from terminator
getSuccessors :: Terminator -> [BlockId]
getSuccessors Return {} = []
getSuccessors Jump {jumpTarget} = [jumpTarget]
getSuccessors CondJump {condTrueBlockId, condFalseBlockId} = [condTrueBlockId, condFalseBlockId]

performLivenessAnalysis :: CFG -> LivenessInfo
performLivenessAnalysis cfg = fixedPoint blockMap predecessors initialLiveIn initialLiveOut
  where
    blockList = cfgBlocks cfg
    blockMap = Map.fromList [(cfgBlockId b, b) | b <- blockList]
    predecessors = buildPredecessorMap cfg
      where
        -- Build predecessor map for CFG
        buildPredecessorMap :: CFG -> Map BlockId [BlockId]
        buildPredecessorMap CFG {cfgBlocks} = foldr addEdges Map.empty cfgBlocks
          where
            addEdges BasicBlock {blockTerminator, cfgBlockId} acc =
              foldr (\succ -> Map.insertWith (++) succ [cfgBlockId]) acc successors
              where
                successors = getSuccessors blockTerminator

    -- Initialize all sets to empty
    initialLiveIn = Map.fromList [(cfgBlockId b, Set.empty) | b <- blockList]
    initialLiveOut = Map.fromList [(cfgBlockId b, Set.empty) | b <- blockList]

    fixedPoint ::
      Map BlockId BasicBlock ->
      Map BlockId [BlockId] ->
      Map BlockId LiveSet ->
      Map BlockId LiveSet ->
      LivenessInfo
    fixedPoint blockMap predecessors liveIn liveOut =
      if newLiveIn == liveIn && newLiveOut == liveOut
        then LivenessInfo {livenessIn = newLiveIn, livenessOut = newLiveOut}
        else fixedPoint blockMap predecessors newLiveIn newLiveOut
      where
        (newLiveIn, newLiveOut) = oneIteration blockMap predecessors liveIn liveOut

        oneIteration ::
          Map BlockId BasicBlock ->
          Map BlockId [BlockId] ->
          Map BlockId LiveSet ->
          Map BlockId LiveSet ->
          (Map BlockId LiveSet, Map BlockId LiveSet)
        oneIteration blockMap predecessors oldLiveIn oldLiveOut = (newLiveIn, newLiveOut)
          where
            blockIds = Map.keys blockMap
            (newLiveIn, newLiveOut) =
              foldr
                (updateBlock blockMap predecessors)
                (oldLiveIn, oldLiveOut)
                blockIds

            updateBlock ::
              Map BlockId BasicBlock ->
              Map BlockId [BlockId] ->
              BlockId ->
              (Map BlockId LiveSet, Map BlockId LiveSet) ->
              (Map BlockId LiveSet, Map BlockId LiveSet)
            updateBlock blockMap predecessors blockId (liveIn, liveOut) =
              case Map.lookup blockId blockMap of
                Nothing -> (liveIn, liveOut)
                Just block -> (Map.insert blockId newIn liveIn, Map.insert blockId newOut liveOut)
                  where
                    -- OUT[B] = union of IN[S] for all successors S of B
                    successors = getSuccessors (blockTerminator block)
                    newOut = Set.unions [Map.findWithDefault Set.empty s liveIn | s <- successors]

                    -- Calculate used and defined temps for this block
                    (used, defined) = analyzeBlock block

                    -- Entry blocks should always have empty IN sets
                    -- IN[B] = USE[B] ∪ (OUT[B] - DEF[B]) for non-entry blocks
                    isEntryBlock = null (Map.findWithDefault [] blockId predecessors)
                    newIn =
                      if isEntryBlock
                        then Set.empty
                        else
                          used `Set.union` (newOut `Set.difference` defined)
              where
                -- Analyze a block to get variables used before
                -- and variables defined in it (defined)
                -- This is done in reverse order to ensure liveness is calculated correctly
                analyzeBlock :: BasicBlock -> (Set Temp, Set Temp)
                analyzeBlock (BasicBlock _ insts term) = (used, defined)
                  where
                    termUses = getTerminatorUses term
                    (used, defined) = foldr processInstruction (termUses, Set.empty) insts

                    processInstruction :: Inst -> (Set Temp, Set Temp) -> (Set Temp, Set Temp)
                    processInstruction inst (used, defined) = (newUsed, newDefined)
                      where
                        -- `used` are the temps used before definition until now in the block (reverse order)
                        -- `defined` are the temps defined in the block
                        instUses = getUsedTemps inst
                        instDefs = getDefinedTemps inst
                        -- ReAssigne newly defined temps from used set, add new uses
                        newUsed = (used `Set.difference` instDefs) `Set.union` instUses
                        newDefined = defined `Set.union` instDefs

getLiveAtInstruction :: LivenessInfo -> BlockId -> Inst -> Set Temp
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
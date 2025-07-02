{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Mir.Interference where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Mir.Liveness
import Mir.Types

-- Interference graph: each temp maps to set of interfering temps
type InterferenceGraph = Map Temp (Set Temp)

-- Build interference graph from liveness info
buildInterferenceGraph :: CFG -> LivenessInfo -> InterferenceGraph
buildInterferenceGraph cfg liveness =
  let allTemps = getAllTemps cfg
      initialGraph = Map.fromList [(t, Set.empty) | t <- Set.toList allTemps]
   in foldr (addBlockInterferences liveness) initialGraph (cfgBlocks cfg)
  where
    -- Add interferences for a single block
    addBlockInterferences :: LivenessInfo -> BasicBlock -> InterferenceGraph -> InterferenceGraph
    addBlockInterferences liveness block graph =
      let blockId' = cfgBlockId block
          liveOutSet = Map.findWithDefault Set.empty blockId' (livenessOut liveness)
          -- Add interferences at block exit
          graphWithExit = addInterferences liveOutSet graph
       in -- Process instructions backwards
          foldr (addInstInterferences liveness blockId') graphWithExit (reverse (blockInsts block))
      where
        -- Add interferences for a single instruction
        addInstInterferences :: LivenessInfo -> BlockId -> Inst -> InterferenceGraph -> InterferenceGraph
        addInstInterferences _liveness _blockId inst graph =
          let defined = getDefinedTemps inst
              used = getUsedTemps inst
           in -- A defined temp interferes with all live temps (except itself)
              foldr (\def -> addTempInterferences def (used <> defined)) graph (Set.toList defined)

        -- Add all pairwise interferences in a set
        addInterferences :: Set Temp -> InterferenceGraph -> InterferenceGraph
        addInterferences temps graph =
          let tempList = Set.toList temps
           in foldr
                ( \t1 acc ->
                    foldr
                      ( \t2 ->
                          if t1 /= t2
                            then addInterference t1 t2
                            else id
                      )
                      acc
                      tempList
                )
                graph
                tempList

        -- Add interference between two temps
        addInterference :: Temp -> Temp -> InterferenceGraph -> InterferenceGraph
        addInterference t1 t2 graph = Map.insertWith Set.union t2 (Set.singleton t1) graph'
          where
            graph' = Map.insertWith Set.union t1 (Set.singleton t2) graph

        -- Add interferences between one temp and a set of temps
        addTempInterferences :: Temp -> Set Temp -> InterferenceGraph -> InterferenceGraph
        addTempInterferences temp interferers graph =
          foldr (addInterference temp) graph (Set.toList interferers)

getAllTemps :: CFG -> Set Temp
getAllTemps cfg =
  Set.unions [getBlockTemps block | block <- cfgBlocks cfg]
  where
    getBlockTemps block =
      let instTemps = Set.unions [getInstTemps inst | inst <- blockInsts block]
          termTemps = getTerminatorUses (blockTerminator block)
       in instTemps <> termTemps

    getInstTemps inst = getUsedTemps inst <> getDefinedTemps inst

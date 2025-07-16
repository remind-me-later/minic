{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}

module Mir.Recursion where

import Data.Functor.Foldable
import Data.Set (Set)
import Data.Set qualified as Set
import Mir.Types
import TypeSystem

-- Base functors for recursion schemes
data OperandF r
  = ConstIntF Int
  | ConstCharF Char
  | TempOperandF Temp
  | RegOperandF Register
  | StackOperandF Int
  | DataOperandF Int
  deriving (Eq, Show, Functor, Foldable, Traversable)

data InstF r
  = AssignF RecOperand RecOperand
  | UnaryOpF RecOperand UnaryOp RecOperand
  | BinOpF RecOperand BinOp RecOperand RecOperand
  | CallF (Maybe RecOperand) Id Int
  | ParamF RecOperand
  deriving (Eq, Show, Functor, Foldable, Traversable)

data TerminatorF r
  = ReturnF (Maybe RecOperand)
  | JumpF BasicBlockId
  | CondJumpF RecOperand BasicBlockId BasicBlockId
  deriving (Eq, Show, Functor, Foldable, Traversable)

data BasicBlockF r
  = BasicBlockF BasicBlockId [RecInst] RecTerminator
  deriving (Eq, Show, Functor, Foldable, Traversable)

data CFGF r
  = CFGF BasicBlockId (Set BasicBlockId) [r]
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- Newtype wrappers to avoid orphan instances
newtype RecOperand = RecOperand Operand
  deriving (Eq, Show)

newtype RecInst = RecInst Inst
  deriving (Eq, Show)

newtype RecTerminator = RecTerminator Terminator
  deriving (Eq, Show)

newtype RecBasicBlock = RecBasicBlock BasicBlock
  deriving (Eq, Show)

-- Utility functions to convert between wrapped and unwrapped types
fromRecOperand :: RecOperand -> Operand
fromRecOperand (RecOperand op) = op

toRecOperand :: Operand -> RecOperand
toRecOperand = RecOperand

fromRecInst :: RecInst -> Inst
fromRecInst (RecInst inst) = inst

toRecInst :: Inst -> RecInst
toRecInst = RecInst

fromRecTerminator :: RecTerminator -> Terminator
fromRecTerminator (RecTerminator term) = term

toRecTerminator :: Terminator -> RecTerminator
toRecTerminator = RecTerminator

fromRecBasicBlock :: RecBasicBlock -> BasicBlock
fromRecBasicBlock (RecBasicBlock bb) = bb

toRecBasicBlock :: BasicBlock -> RecBasicBlock
toRecBasicBlock = RecBasicBlock

-- Define recursion instances for wrapped types
type instance Base RecOperand = OperandF

instance Recursive RecOperand where
  project (RecOperand (ConstInt n)) = ConstIntF n
  project (RecOperand (ConstChar c)) = ConstCharF c
  project (RecOperand (TempOperand t)) = TempOperandF t
  project (RecOperand (RegOperand r)) = RegOperandF r
  project (RecOperand (StackOperand n)) = StackOperandF n
  project (RecOperand (DataOperand n)) = DataOperandF n

instance Corecursive RecOperand where
  embed (ConstIntF n) = RecOperand (ConstInt n)
  embed (ConstCharF c) = RecOperand (ConstChar c)
  embed (TempOperandF t) = RecOperand (TempOperand t)
  embed (RegOperandF r) = RecOperand (RegOperand r)
  embed (StackOperandF n) = RecOperand (StackOperand n)
  embed (DataOperandF n) = RecOperand (DataOperand n)

type instance Base RecInst = InstF

instance Recursive RecInst where
  project (RecInst (Assign dst src)) = AssignF (toRecOperand dst) (toRecOperand src)
  project (RecInst (UnaryOp dst op src)) = UnaryOpF (toRecOperand dst) op (toRecOperand src)
  project (RecInst (BinOp dst op left right)) = BinOpF (toRecOperand dst) op (toRecOperand left) (toRecOperand right)
  project (RecInst (Call ret funId argCount)) = CallF (fmap toRecOperand ret) funId argCount
  project (RecInst (Param operand)) = ParamF (toRecOperand operand)

instance Corecursive RecInst where
  embed (AssignF dst src) = RecInst (Assign (fromRecOperand dst) (fromRecOperand src))
  embed (UnaryOpF dst op src) = RecInst (UnaryOp (fromRecOperand dst) op (fromRecOperand src))
  embed (BinOpF dst op left right) = RecInst (BinOp (fromRecOperand dst) op (fromRecOperand left) (fromRecOperand right))
  embed (CallF ret funId argCount) = RecInst (Call (fmap fromRecOperand ret) funId argCount)
  embed (ParamF operand) = RecInst (Param (fromRecOperand operand))

type instance Base RecTerminator = TerminatorF

instance Recursive RecTerminator where
  project (RecTerminator (Return operand)) = ReturnF (fmap toRecOperand operand)
  project (RecTerminator (Jump target)) = JumpF target
  project (RecTerminator (CondJump cond trueId falseId)) = CondJumpF (toRecOperand cond) trueId falseId

instance Corecursive RecTerminator where
  embed (ReturnF operand) = RecTerminator (Return (fmap fromRecOperand operand))
  embed (JumpF target) = RecTerminator (Jump target)
  embed (CondJumpF cond trueId falseId) = RecTerminator (CondJump (fromRecOperand cond) trueId falseId)

type instance Base RecBasicBlock = BasicBlockF

instance Recursive RecBasicBlock where
  project (RecBasicBlock (BasicBlock blockId insts term)) =
    BasicBlockF blockId (map toRecInst insts) (toRecTerminator term)

instance Corecursive RecBasicBlock where
  embed (BasicBlockF blockId insts term) =
    RecBasicBlock (BasicBlock blockId (map fromRecInst insts) (fromRecTerminator term))

-- Algebra for collecting temporaries
type TempSet = Set Temp

-- Catamorphism to collect all temporaries from an operand
collectOperandTemps :: OperandF TempSet -> TempSet
collectOperandTemps (TempOperandF t) = Set.singleton t
collectOperandTemps _ = Set.empty

-- Catamorphism to collect used temporaries from an instruction
collectUsedTemps :: InstF TempSet -> TempSet
collectUsedTemps (AssignF _ src) = getRecOperandTemps src
collectUsedTemps (UnaryOpF _ _ src) = getRecOperandTemps src
collectUsedTemps (BinOpF _ _ left right) = getRecOperandTemps left <> getRecOperandTemps right
collectUsedTemps (CallF {}) = Set.empty
collectUsedTemps (ParamF operand) = getRecOperandTemps operand

-- Catamorphism to collect defined temporaries from an instruction
collectDefinedTemps :: InstF TempSet -> TempSet
collectDefinedTemps (AssignF dst _) = getRecOperandTemps dst
collectDefinedTemps (UnaryOpF dst _ _) = getRecOperandTemps dst
collectDefinedTemps (BinOpF dst _ _ _) = getRecOperandTemps dst
collectDefinedTemps (CallF (Just ret) _ _) = getRecOperandTemps ret
collectDefinedTemps _ = Set.empty

-- Catamorphism to collect temporaries from terminator
collectTerminatorTemps :: TerminatorF TempSet -> TempSet
collectTerminatorTemps (CondJumpF cond _ _) = getRecOperandTemps cond
collectTerminatorTemps (ReturnF (Just operand)) = getRecOperandTemps operand
collectTerminatorTemps _ = Set.empty

-- Helper functions using recursion schemes on wrapped types
getRecOperandTemps :: RecOperand -> Set Temp
getRecOperandTemps = cata collectOperandTemps

getOperandTemps :: Operand -> Set Temp
getOperandTemps = getRecOperandTemps . toRecOperand

getUsedTempsRec :: Inst -> Set Temp
getUsedTempsRec = cata collectUsedTemps . toRecInst

getDefinedTempsRec :: Inst -> Set Temp
getDefinedTempsRec = cata collectDefinedTemps . toRecInst

getTerminatorTempsRec :: Terminator -> Set Temp
getTerminatorTempsRec = cata collectTerminatorTemps . toRecTerminator

-- Liveness analysis using recursion schemes
data LivenessAlgebra = LivenessAlgebra
  { used :: Set Temp,
    defined :: Set Temp
  }

instance Semigroup LivenessAlgebra where
  LivenessAlgebra u1 d1 <> LivenessAlgebra u2 d2 =
    LivenessAlgebra (u1 <> u2) (d1 <> d2)

instance Monoid LivenessAlgebra where
  mempty = LivenessAlgebra Set.empty Set.empty

-- Catamorphism for liveness analysis of a single instruction
livenessInstAlgebra :: InstF LivenessAlgebra -> LivenessAlgebra
livenessInstAlgebra instF = LivenessAlgebra usedTemps definedTemps
  where
    usedTemps = case instF of
      AssignF _ src -> getRecOperandTemps src
      UnaryOpF _ _ src -> getRecOperandTemps src
      BinOpF _ _ left right -> getRecOperandTemps left <> getRecOperandTemps right
      CallF {} -> Set.empty
      ParamF operand -> getRecOperandTemps operand

    definedTemps = case instF of
      AssignF dst _ -> getRecOperandTemps dst
      UnaryOpF dst _ _ -> getRecOperandTemps dst
      BinOpF dst _ _ _ -> getRecOperandTemps dst
      CallF (Just ret) _ _ -> getRecOperandTemps ret
      _ -> Set.empty

-- Analyze instruction liveness using catamorphism
analyzeInstLiveness :: Inst -> LivenessAlgebra
analyzeInstLiveness = cata livenessInstAlgebra . toRecInst

-- Block-level liveness analysis using fold
analyzeBlockLiveness :: BasicBlock -> (Set Temp, Set Temp)
analyzeBlockLiveness (BasicBlock _ insts terminator) = (totalUsed, totalDefined)
  where
    -- Analyze instructions in reverse order for proper liveness calculation
    instAnalysis = map analyzeInstLiveness (reverse insts)
    termUsed = getTerminatorTempsRec terminator

    -- Fold over instructions to compute block liveness
    (totalUsed, totalDefined) = foldr combineInstLiveness (termUsed, Set.empty) instAnalysis

    combineInstLiveness :: LivenessAlgebra -> (Set Temp, Set Temp) -> (Set Temp, Set Temp)
    combineInstLiveness (LivenessAlgebra instUsed instDefined) (blockUsed, blockDefined) =
      ( (blockUsed `Set.difference` instDefined) `Set.union` instUsed,
        blockDefined `Set.union` instDefined
      )

-- Transform operations using recursion schemes
-- Anamorphism to replace temporaries
replaceTempsInOperand :: (Temp -> Temp) -> Operand -> Operand
replaceTempsInOperand f operand = fromRecOperand $ ana coalgebra (toRecOperand operand)
  where
    coalgebra (RecOperand op) = case op of
      TempOperand t -> TempOperandF (f t)
      ConstInt n -> ConstIntF n
      ConstChar c -> ConstCharF c
      RegOperand r -> RegOperandF r
      StackOperand n -> StackOperandF n
      DataOperand n -> DataOperandF n

-- Paramorphism to transform instructions while keeping original structure
transformInstWithContext :: (Inst -> InstF RecInst -> Inst) -> Inst -> Inst
transformInstWithContext f inst = fromRecInst $ para algebra (toRecInst inst)
  where
    algebra instF = toRecInst $ f (fromRecInst $ embed (fmap fst instF)) (fmap snd instF)

-- Additional utility functions for working with wrapped types
mapOperandsInInst :: (Operand -> Operand) -> Inst -> Inst
mapOperandsInInst f = fromRecInst . cata algebra . toRecInst
  where
    algebra :: InstF RecInst -> RecInst
    algebra instF = case instF of
      AssignF dst src -> toRecInst $ Assign (f $ fromRecOperand dst) (f $ fromRecOperand src)
      UnaryOpF dst op src -> toRecInst $ UnaryOp (f $ fromRecOperand dst) op (f $ fromRecOperand src)
      BinOpF dst op left right -> toRecInst $ BinOp (f $ fromRecOperand dst) op (f $ fromRecOperand left) (f $ fromRecOperand right)
      CallF ret funId argCount -> toRecInst $ Call (fmap (f . fromRecOperand) ret) funId argCount
      ParamF operand -> toRecInst $ Param (f $ fromRecOperand operand)

mapOperandsInTerminator :: (Operand -> Operand) -> Terminator -> Terminator
mapOperandsInTerminator f = fromRecTerminator . cata algebra . toRecTerminator
  where
    algebra :: TerminatorF RecTerminator -> RecTerminator
    algebra termF = case termF of
      ReturnF operand -> toRecTerminator $ Return (fmap (f . fromRecOperand) operand)
      JumpF target -> toRecTerminator $ Jump target
      CondJumpF cond trueId falseId -> toRecTerminator $ CondJump (f $ fromRecOperand cond) trueId falseId

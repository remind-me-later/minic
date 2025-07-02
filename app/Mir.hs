module Mir
  ( module Mir.Types,
    module Mir.Translate,
    module Mir.Liveness,
    module Mir.Allocation,
  )
where

import Mir.Allocation
  ( AllocationResult (..),
    RegisterAssignment,
    SpillLocation (..),
    allocateProgram,
  )
import Mir.Liveness
  ( LivenessInfo (..),
    analyzeFunctionLiveness,
    analyzeProgramLiveness,
  )
import Mir.Translate
import Mir.Types

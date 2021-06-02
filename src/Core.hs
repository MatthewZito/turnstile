{-|
Module      : Core
Description : State machine for Build pipelines
License     : GPL-3
Maintainer  : exbotanical at protonmail
Stability   : experimental
Portability : POSIX
-}
module Core where

import RIO
import qualified RIO.Map as Map
import qualified RIO.List as List

import qualified Docker

-- *Pipeline Types

-- |A Pipeline describes a sequence of ordered steps for a build
data Pipeline
  = Pipeline
  {
    steps :: NonEmpty Step
  } deriving (Eq, Show)

-- *Pipeline Step Types

-- |A single step in the BuildSteps sequence
data Step
  = Step
  {
    name :: StepName
    , image :: Docker.Image
    , commands :: NonEmpty [Text]
  } deriving (Eq, Show)

-- |A step result - contingent on return code; either successful or erroneous
data StepResult
  = StepFailed Docker.ContainerReturnCode
  | StepSucceeded
  deriving (Eq, Show)

newtype StepName = StepName Text
  -- ^We derive `Ord` so we can use as a map key
  deriving (Eq, Show, Ord)

-- *Pipeline Build Types

-- |Core Build type
data Build
  = Build
  {
    pipeline :: Pipeline
    , state :: BuildState
    , completedSteps :: Map StepName StepResult
  }

-- |The current build state, deterministic; state-machined
data BuildState
  = BuildReady
  | BuildRunning BuildRunningState
  | BuildFinished BuildResult
  deriving (Eq, Show)

-- |Stores data about the current running step
data BuildRunningState
  = BuildRunningState
  {
    step :: StepName
  } deriving (Eq, Show)

-- |Result of the build state sequence
data BuildResult
  = BuildSucceeded
  | BuildFailed
  deriving (Eq, Show)

-- *Helpers

-- |Extract the text from a StepName
stepNameToText :: StepName -> Text
stepNameToText ( StepName step) = step

-- |Determine the StepResult contingent on the ContainerReturnCode
returnCodeToStepResult :: Docker.ContainerReturnCode -> StepResult
returnCodeToStepResult rc =
  if Docker.returnCodeToInt rc == 0
    then StepSucceeded
    else StepFailed rc

-- |Determine if we've another BuildStep to process
buildHasNextStep :: Build -> Either BuildResult Step
buildHasNextStep build =
  if allSucceeded
    then case nextStep of
      Just step -> Right step
      Nothing -> Left BuildSucceeded
    else Left BuildFailed
  where
    allSucceeded = List.all ((==) StepSucceeded) build.completedSteps
    nextStep = List.find f build.pipeline.steps
    -- *Get next non-completed step i.e. not in the `completedSteps` map
    f step = not $ Map.member step.name build.completedSteps

-- *State Machines

-- |Given a Build, deterministically output the next state
-- *State machine, produces side effects
progress :: Build -> IO Build
progress build =
  case build.state of
    BuildReady ->
      case buildHasNextStep build of
        Left result ->
          pure $ build { state = BuildFinished result }
        Right step -> do
          let s = BuildRunningState { step = step.name }
          pure $ build { state = BuildRunning s }

    BuildRunning state -> do
      let return = Docker.ContainerReturnCode 0
          result = returnCodeToStepResult return

      pure build
        {
          state = BuildReady
          , completedSteps
            = Map.insert state.step result build.completedSteps
        }

    BuildFinished _ -> undefined

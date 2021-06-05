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
import qualified RIO.Text as Text
import qualified RIO.NonEmpty as NonEmpty
import qualified Data.Aeson as Aeson

import qualified Docker



-- *Pipeline Types


-- |A Pipeline describes a sequence of ordered steps for a build
data Pipeline
  = Pipeline
    {
      steps :: NonEmpty Step
    } deriving (Eq, Show, Generic, Aeson.FromJSON)



-- *Pipeline Step Types


-- |A single step in the BuildSteps sequence
data Step
  = Step
    {
      name :: StepName
      , image :: Docker.Image
      , commands :: NonEmpty Text
    } deriving (Eq, Show, Generic, Aeson.FromJSON)


-- |A step result - contingent on return code; either successful or erroneous
data StepResult
  = StepFailed Docker.ContainerReturnCode
  | StepSucceeded
  deriving (Eq, Show)

newtype StepName = StepName Text
  -- ^We derive `Ord` so we can use as a map key
  deriving (Eq, Show, Ord, Generic, Aeson.FromJSON)



-- *Pipeline Build Types


-- |Core Build type
data Build
  = Build
    {
      pipeline :: Pipeline
      , state :: BuildState
      , completedSteps :: Map StepName StepResult
      , volume :: Docker.Volume
    } deriving (Eq, Show)


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
      , container :: Docker.ContainerId
    } deriving (Eq, Show)


-- |Result of the build state sequence
data BuildResult
  = BuildSucceeded
  | BuildFailed
  | BuildUnexpectedState Text
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
-- |State machine, produces side effects
progress :: Docker.Service -> Build -> IO Build
progress srv build =
  case build.state of

    BuildReady ->
      case buildHasNextStep build of
        Left result ->
          pure $ build { state = BuildFinished result }
        Right step -> do
          let script = Text.unlines
                $ ["set -ex"] <> NonEmpty.toList step.commands
          -- create the container
          let options =
                Docker.CreateContainerOptions
                  {
                    image = step.image
                    , script = script
                    , volume = build.volume
                  }

          srv.pullImage step.image
          container <- srv.createContainer options
          srv.startContainer container

          -- proceed by recursing the state machine
          let s = BuildRunningState
                  {
                    step = step.name
                    , container = container
                  }

          pure $ build { state = BuildRunning s }

    BuildRunning state -> do

      status <- srv.containerStatus state.container

      case status of
        Docker.ContainerRunning ->
          -- wait for the container to exit
          pure build

        Docker.ContainerExited ret -> do
          let result = returnCodeToStepResult ret
          pure build
            {
              state = BuildReady
              , completedSteps = Map.insert state.step result build.completedSteps
            }

        Docker.ContainerOther other -> do
          let result = BuildUnexpectedState other
          pure build { state = BuildFinished result }

    BuildFinished _ -> undefined

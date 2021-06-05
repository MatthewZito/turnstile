{-|
Module      : Logger
Description : State machine for log collection processes
License     : GPL-3
Maintainer  : exbotanical at protonmail
Stability   : experimental
Portability : POSIX
-}
module Logger where

import RIO

import qualified RIO.Map as Map
import qualified RIO.NonEmpty as NonEmpty
import qualified Data.Time.Clock.POSIX as Time

import qualified Core
import qualified Docker



-- *Types


-- |Tracks the status of logs collection processes
data CollectionStatus
  = CollectionReady
  -- ^Container has not started yet
  | CollectingLogs Docker.ContainerId Time.POSIXTime
  -- ^Container is running
  | CollectionFinished
  -- ^Container has exited
  deriving (Eq, Show)


-- |Stores log metadata
data Log = Log
  {
    output :: ByteString
    , step :: Core.StepName
  }


-- |Mapped logs
type LogCollection = Map Core.StepName CollectionStatus



-- *Actions


-- |Returns tuple IO of the new log state and log lines collected during a given time period
collectLogs
  :: Docker.Service
  -> LogCollection
  -> Core.Build
  -> IO (LogCollection, [Log])
collectLogs srv coll build = do
  now <- Time.getPOSIXTime
  logs <- runCollection srv now coll
  let newColl = updateCollection build.state now coll
  pure (newColl, logs)


-- Performs the log collection
runCollection
  :: Docker.Service
  -> Time.POSIXTime
  -> LogCollection
  -> IO [Log]
runCollection srv until coll = do
  logs <- Map.traverseWithKey f coll
  pure $ concat (Map.elems logs)
  where
    f step = \case
      CollectionReady -> pure []
      CollectionFinished -> pure []
      CollectingLogs container since -> do
        let options =
              Docker.FetchLogsOptions
                {
                  container = container
                  , since = since
                  , until = until
                }
        output <- srv.fetchLogs options
        pure [Log { step = step, output = output }]


-- |Initializes a log collection process
initLogCollection :: Core.Pipeline -> LogCollection
initLogCollection pipeline =
  Map.fromList $ NonEmpty.toList steps
  where
    steps = pipeline.steps <&> \step -> (step.name, CollectionReady)


-- |State machine for managing LogCollection states
updateCollection
  :: Core.BuildState
  -> Time.POSIXTime
  -> LogCollection
  -> LogCollection
updateCollection state lastColl coll =
  -- over ea k/v pair in the LogCollection
  Map.mapWithKey f coll
  where
    update step since nextState =
      case state of
        Core.BuildRunning state ->
          if state.step == step
            then CollectingLogs state.container since
            else nextState
        _ -> nextState

    f step = \case
      -- transition to collecting only if step is currently running
      CollectionReady ->
        -- begin initial collection at ts 0
        update step 0 CollectionReady
      CollectingLogs _ _ ->
        update step lastColl CollectionFinished
      CollectionFinished ->
        CollectionFinished

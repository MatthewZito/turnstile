{-|
Module      : Runner
Description : Manages and runs build tasks
License     : GPL-3
Maintainer  : exbotanical at protonmail
Stability   : experimental
Portability : POSIX
-}
module Runner where

import RIO

import Core
import qualified Docker

data Service
  = Service
    {
      prepareBuild :: Pipeline -> IO Build
      , runBuild :: Build -> IO Build
    }

-- |Requires a dependency from which a new service interface is created
createService :: Docker.Service -> IO Service
createService docker = do
  pure Service
    {
      prepareBuild = prepareBuild_ docker
      , runBuild = runBuild_ docker
    }

prepareBuild_ :: Docker.Service -> Pipeline -> IO Build
prepareBuild_ docker pipeline = do
  pure Build
    {
      pipeline = pipeline
      , state = BuildReady
      , completedSteps = mempty
    }

-- |Given a build, bootstraps a new process as a state machine transaction
runBuild_ :: Docker.Service -> Build -> IO Build
runBuild_ docker build = do
  newBuild <- Core.progress docker build
  case newBuild.state of
    BuildFinished _ ->
      pure newBuild
    _ -> do
      threadDelay (1 * 1000 * 1000)
      runBuild_ docker newBuild

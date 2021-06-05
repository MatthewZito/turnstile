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
import qualified Logger



-- *Types

-- |Injectable service instance wrapper
data Service
  = Service
    {
      prepareBuild :: Pipeline -> IO Build
      , runBuild :: Hooks -> Build -> IO Build
    }


-- |Allows caller to supply a callback to rubBuild
data Hooks
  = Hooks
  {
    logCollected :: Logger.Log -> IO ()
  }



-- *Actions


-- |Requires a dependency from which a new service interface is created
createService :: Docker.Service -> IO Service
createService docker = do
  pure Service
    {
      prepareBuild = prepareBuild_ docker
      , runBuild = runBuild_ docker
    }

-- |Assemble a build task
prepareBuild_ :: Docker.Service -> Pipeline -> IO Build
prepareBuild_ docker pipeline = do
  volume <- docker.createVolume
  pure Build
    {
      pipeline = pipeline
      , state = BuildReady
      , completedSteps = mempty
      , volume = volume
    }


-- |Given a build, bootstraps a new process as a state machine transaction
runBuild_ :: Docker.Service -> Hooks -> Build -> IO Build
runBuild_ docker hooks build = do
  recurse build $ Logger.initLogCollection build.pipeline
  where
    recurse :: Build -> Logger.LogCollection -> IO Build
    recurse build collection = do

      (newColl, logs) <- Logger.collectLogs docker collection build
      traverse_ hooks.logCollected logs

      newBuild <- Core.progress docker build
      case newBuild.state of
        BuildFinished _ ->
          pure newBuild
        _ -> do
          threadDelay (1 * 1000 * 1000)
          recurse newBuild newColl

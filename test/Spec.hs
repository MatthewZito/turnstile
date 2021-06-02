module Main where

import RIO
import Test.Hspec
import qualified RIO.Map as Map
import qualified RIO.NonEmpty.Partial as Partial

import Core
import qualified Docker

-- Helpers

makeStep :: Text -> Text -> [Text] -> Step
makeStep name image commands
  = Step
  {
    name = StepName name
    , image = Docker.Image image
    , commands = Partial.fromList commands
  }

makePipeline :: [Step] -> Pipeline
makePipeline steps =
  Pipeline { steps = Partial.fromList steps }

-- |Setup

testPipeline :: Pipeline
testPipeline =
  makePipeline [
    makeStep "First step" "ubuntu" ["date"]
    , makeStep "Second step" "ubuntu" ["uname -r"]
  ]

testBuild :: Build
testBuild = Build
  {
    pipeline = testPipeline
    , state = BuildReady
    , completedSteps = mempty
  }

runBuild :: Docker.Service -> Build -> IO Build
runBuild docker build = do
  newBuild <- Core.progress docker build
  case newBuild.state of
    BuildFinished _ ->
      pure newBuild
    _ -> do
      -- recurse until we've reached a `BuildFinished` state
      threadDelay (1 * 1000 * 1000)
      runBuild docker newBuild

-- *Tests

testRunSuccess :: Docker.Service -> IO ()
testRunSuccess docker = do
  result <- runBuild docker testBuild
  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [StepSucceeded, StepSucceeded]

main :: IO ()
main = hspec do
  docker <- runIO Docker.createService
  describe "Turnstile" do
    it "should run a successful build" do
      testRunSuccess docker

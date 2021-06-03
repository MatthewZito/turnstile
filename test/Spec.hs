module Main where

import RIO
import Test.Hspec
import qualified RIO.Map as Map
import qualified RIO.NonEmpty.Partial as Partial
import qualified System.Process.Typed as Process

import Core
import qualified Docker
import qualified Runner


-- *Helpers

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


-- *Setup

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


-- *Setup + Teardown

cleanDocker :: IO ()
cleanDocker = void do
  Process.readProcessStdout "docker rm -f $(docker ps -aq --filter \"label=turnstile\")"


-- *Tests

testRunSuccess :: Runner.Service -> IO ()
testRunSuccess runner = do
  build <- runner.prepareBuild $ makePipeline
    [
      makeStep "Step one" "ubuntu" ["date"]
      , makeStep "Step two" "ubuntu" ["uname -r"]
    ]
  result <- runner.runBuild build

  result.state `shouldBe` BuildFinished BuildSucceeded
  Map.elems result.completedSteps `shouldBe`
    [
      StepSucceeded
      , StepSucceeded
    ]

testRunFailure :: Runner.Service -> IO ()
testRunFailure runner = do
  build <- runner.prepareBuild $ makePipeline
    [
      makeStep "Step one" "ubuntu" ["date"]
      , makeStep "Step two" "ubuntu" ["exit 1"]
    ]
  result <- runner.runBuild build

  result.state `shouldBe` BuildFinished BuildFailed
  Map.elems result.completedSteps `shouldBe`
    [
      StepSucceeded
      , StepFailed (Docker.ContainerReturnCode 1)
    ]

-- *Test Exec

main :: IO ()
main = hspec do
  docker <- runIO Docker.createService
  runner <- runIO $ Runner.createService docker

  beforeAll cleanDocker $ describe "Turnstile" do
    it "should run a successful build" do
      testRunSuccess runner
    it "should run an errorenous build" do
      testRunFailure runner

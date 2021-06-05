module Main where

import RIO
import Test.Hspec

import qualified RIO.Map as Map
import qualified RIO.NonEmpty.Partial as Partial
import qualified RIO.ByteString as ByteString
import qualified RIO.Set as Set
import qualified Data.Yaml as Yaml
import qualified System.Process.Typed as Process

import qualified Core
import qualified Docker
import qualified Runner
import qualified Logger



-- *Helpers


makeStep :: Text -> Text -> [Text] -> Core.Step
makeStep name image commands
  = Core.Step
  {
    name = Core.StepName name
    , image = Docker.Image { name = image, tag = "latest" }
    , commands = Partial.fromList commands
  }


makePipeline :: [Core.Step] -> Core.Pipeline
makePipeline steps =
  Core.Pipeline { steps = Partial.fromList steps }


emptyHooks :: Runner.Hooks
emptyHooks =
  Runner.Hooks
    {
      logCollected = \_ -> pure ()
    }



-- *Setup + Teardown


runBuild :: Docker.Service -> Core.Build -> IO Core.Build
runBuild docker build = do
  newBuild <- Core.progress docker build
  case newBuild.state of
    Core.BuildFinished _ ->
      pure newBuild
    _ -> do
      -- recurse until we've reached a `BuildFinished` state
      threadDelay (1 * 1000 * 1000)
      runBuild docker newBuild


cleanDocker :: IO ()
cleanDocker = void do
  Process.readProcessStdout "docker rm -f $(docker ps -aq --filter \"label=turnstile\")"
  Process.readProcessStdout "docker volume rm -f $(docker volume ls -q --filter \"label=turnstile\")"



-- *Tests


testRunSuccess :: Runner.Service -> IO ()
testRunSuccess runner = do
  build <- runner.prepareBuild $ makePipeline
    [
      makeStep "Step one" "ubuntu" ["date"]
      , makeStep "Step two" "ubuntu" ["uname -r"]
    ]
  result <- runner.runBuild emptyHooks build

  result.state `shouldBe` Core.BuildFinished Core.BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [Core.StepSucceeded, Core.StepSucceeded]


testRunFailure :: Runner.Service -> IO ()
testRunFailure runner = do
  build <- runner.prepareBuild $ makePipeline
    [
      makeStep "Step one" "ubuntu" ["date"]
      , makeStep "Step two" "ubuntu" ["exit 1"]
    ]
  result <- runner.runBuild emptyHooks build

  result.state `shouldBe` Core.BuildFinished Core.BuildFailed
  Map.elems result.completedSteps `shouldBe` [Core.StepSucceeded, Core.StepFailed (Docker.ContainerReturnCode 1)]


testSharedWorkspace :: Docker.Service -> Runner.Service -> IO ()
testSharedWorkspace docker runner = do
  build <- runner.prepareBuild $ makePipeline
        [
          makeStep "Create file" "ubuntu" ["echo 123 > testf"]
          , makeStep "Read file" "ubuntu" ["cat testf"]
        ]

  result <- runner.runBuild emptyHooks build
  result.state `shouldBe` Core.BuildFinished Core.BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [Core.StepSucceeded, Core.StepSucceeded]


testLogCollection :: Runner.Service -> IO ()
testLogCollection runner = do

  -- maintain set of words expected in log output
  expected <- newMVar $ Set.fromList ["test1", "test2", "Linux"]

  -- recv `Log` value whenever container writes to stdout
  let onLog :: Logger.Log -> IO ()
      onLog log = do
        -- get remaining els in set by reading the mutable var
        remaining <- readMVar expected
        -- iter over ea word in `expected`
        forM_ remaining $ \word -> do
          -- check if in `log.output`
          case ByteString.breakSubstring word log.output of
            -- not found
            (_, "") -> pure ()
            -- update `MVar` w/ new Set that has the found word removed
            _ -> modifyMVar_ expected (pure . Set.delete word)

  let hooks = Runner.Hooks { logCollected = onLog }

  build <- runner.prepareBuild $ makePipeline
    [
      makeStep "Long step" "ubuntu" ["echo test1", "sleep 2", "echo test2"]
      , makeStep "Echo Linux" "ubuntu" ["uname -s"]
    ]
  result <- runner.runBuild hooks build
  result.state `shouldBe` Core.BuildFinished Core.BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [Core.StepSucceeded, Core.StepSucceeded]

  readMVar expected >>= \logs -> logs `shouldBe` Set.empty


testImagePull :: Runner.Service -> IO ()
testImagePull runner = do
  Process.readProcessStdout "docker rmi -f busybox"

  build <- runner.prepareBuild $ makePipeline
    [ makeStep "Initial step" "busybox" ["date"] ]

  result <- runner.runBuild emptyHooks build

  result.state `shouldBe` Core.BuildFinished Core.BuildSucceeded
  Map.elems result.completedSteps `shouldBe` [Core.StepSucceeded]


testYamlDecoding :: Runner.Service -> IO ()
testYamlDecoding runner = do
  pipeline <- Yaml.decodeFileThrow "test/artifacts/pipeline.yml"
  build <- runner.prepareBuild pipeline
  result <- runner.runBuild emptyHooks build
  result.state `shouldBe` Core.BuildFinished Core.BuildSucceeded



-- *Test Exec

main :: IO ()
main = hspec do
  docker <- runIO Docker.createService
  runner <- runIO $ Runner.createService docker

  beforeAll cleanDocker $ describe "Turnstile" do
    it "should run a successful build" do
      testRunSuccess runner
    it "should run an erroneous build" do
      testRunFailure runner
    it "should share a workspace among steps" do
      testSharedWorkspace docker runner
    it "should collect logs" do
      testLogCollection runner
    it "should pull images" do
      testImagePull runner

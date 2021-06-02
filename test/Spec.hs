module Spec where

import RIO
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

main :: IO ()
main = pure ()

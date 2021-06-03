{-|
Module      : Docker
Description : Manages Docker communications via HTTP
License     : GPL-3
Maintainer  : exbotanical at protonmail
Stability   : experimental
Portability : POSIX
-}
module Docker where

import RIO
import qualified Network.HTTP.Simple as HTTP
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import Data.Aeson ((.:))

import qualified SockIO


-- *Data Const

dockerSocketConn :: FilePath
dockerSocketConn = "/var/run/docker.sock"

-- *Types

-- |Docker container env options
data CreateContainerOptions
  = CreateContainerOptions
    {
      image :: Image
    }

-- |Service type helper for dependency injection
data Service
  = Service
    {
      createContainer :: CreateContainerOptions -> IO ContainerId
      , startContainer :: ContainerId -> IO ()
      , containerStatus :: ContainerId -> IO ContainerStatus
    }

-- |Tracks container status
data ContainerStatus
  = ContainerRunning
  | ContainerExited ContainerReturnCode
  | ContainerOther Text
  deriving (Eq, Show)

-- |Stores a Docker image name
newtype Image = Image Text
  deriving (Eq, Show)

-- |Stores the return code of the container step
newtype ContainerReturnCode = ContainerReturnCode Int
  deriving (Eq, Show)

-- |Stores a given Docker container's ID
newtype ContainerId = ContainerId Text
  deriving (Eq, Show)

-- |Initializes an HTTP request
type RequestBuilder = Text -> HTTP.Request


-- *Helpers

-- |Extract the image text from an Image type
imageToText :: Image -> Text
imageToText (Image image) = image

-- |Extract the integer return code from a ContainerReturnCode
returnCodeToInt :: ContainerReturnCode -> Int
returnCodeToInt (ContainerReturnCode code) = code

-- |Convert Docker container ID to text
containerIdToText :: ContainerId -> Text
containerIdToText (ContainerId id) = id

-- |Parse an inbound JSON response object
parseResponse :: HTTP.Response ByteString
  -> (Aeson.Value -> Aeson.Types.Parser a)
  -> IO a
parseResponse res parser = do
  let result = do
        value <- Aeson.eitherDecodeStrict (HTTP.getResponseBody res)
        Aeson.Types.parseEither parser value

  case result of
    Left e -> throwString e
    Right status -> pure status

-- |Create a container service interface
createService :: IO Service
createService = do
  manager <- SockIO.newManager dockerSocketConn

  let newReq :: RequestBuilder
      newReq path =
        HTTP.defaultRequest
        & HTTP.setRequestPath (encodeUtf8 $ "/v1.41" <> path)
        & HTTP.setRequestManager manager

  pure Service
    {
      createContainer = createContainer_ newReq -- inject the request builder
      , startContainer = startContainer_ newReq
      , containerStatus = containerStatus_ newReq
    }

-- |Create a Docker container via the Docker Engine API
createContainer_ :: RequestBuilder -> CreateContainerOptions -> IO ContainerId
createContainer_ newReq options = do
  manager <- SockIO.newManager dockerSocketConn

  let image = imageToText options.image
  let body = Aeson.object
            [
              ("Image", Aeson.toJSON image)
              , ("Tty", Aeson.toJSON True)
              , ("Labels", Aeson.object [("turnstile", "0.1.0.0")])
              , ("Cmd", "echo testing")
              , ("Entrypoint", Aeson.toJSON [Aeson.String "/bin/sh", "-c"])
            ]

  let req = newReq "/containers/create"
            & HTTP.setRequestMethod "POST"
            & HTTP.setRequestBodyJSON body

  let parser = Aeson.withObject "create-container" $ \o -> do
        cId <- o .: "Id"
        pure $ ContainerId cId

  res <- HTTP.httpBS req
  parseResponse res parser

  -- traceShowIO res

-- |Start a Docker container via the Docker Engine API
startContainer_ :: RequestBuilder -> ContainerId -> IO ()
startContainer_ newReq container = do
  let path = "/containers/" <> containerIdToText container <> "/start"

  let req = newReq path
            & HTTP.setRequestMethod "POST"

  void $ HTTP.httpBS req

-- |Resolve the status of a given container
containerStatus_ :: RequestBuilder -> ContainerId -> IO ContainerStatus
containerStatus_ newReq container = do
  let parser = Aeson.withObject "container-inspect" $ \o -> do
      state <- o .: "State"
      status <- state .: "Status"
      case status of
        "running" -> pure ContainerRunning
        "exited" -> do
          code <- state .: "ExitCode"
          pure $ ContainerExited (ContainerReturnCode code)
        other -> pure $ ContainerOther other

  let req = newReq $ "/containers/" <> containerIdToText container <> "/json"

  res <- HTTP.httpBS req
  parseResponse res parser

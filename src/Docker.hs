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
import qualified Data.Time.Clock.POSIX as Time



-- *Data Const

dockerSocketConn :: FilePath
dockerSocketConn = "/var/run/docker.sock"



-- *Types


-- |Docker container env options
data CreateContainerOptions
  = CreateContainerOptions
    {
      image :: Image
      , script :: Text
      , volume :: Volume
    }


-- |Service type helper for dependency injection
data Service
  = Service
    {
      createContainer :: CreateContainerOptions -> IO ContainerId
      , startContainer :: ContainerId -> IO ()
      , containerStatus :: ContainerId -> IO ContainerStatus
      , createVolume :: IO Volume
      , fetchLogs :: FetchLogsOptions -> IO ByteString
      , pullImage :: Image -> IO ()
    }


-- |Tracks container status
data ContainerStatus
  = ContainerRunning
  | ContainerExited ContainerReturnCode
  | ContainerOther Text
  deriving (Eq, Show)


-- |Tracks logs collection cycles
data FetchLogsOptions
  = FetchLogsOptions
    {
      container :: ContainerId
      , since :: Time.POSIXTime
      , until :: Time.POSIXTime
    }


-- |Stores a formatted Docker image name
data Image =
  Image
  {
    name:: Text
    , tag :: Text
  }
  deriving (Eq, Show)


-- |Stores the return code of the container step
newtype ContainerReturnCode = ContainerReturnCode Int
  deriving (Eq, Show)


-- |Stores a given Docker container's ID
newtype ContainerId = ContainerId Text
  deriving (Eq, Show)


-- |Stores necessary data for initializing a volume / shared fs across images
newtype Volume = Volume Text
  deriving (Eq, Show)


-- |Initializes an HTTP request
type RequestBuilder = Text -> HTTP.Request



-- *Helpers


-- |Extract the image text from an Image type
imageToText :: Image -> Text
imageToText image = image.name <> ":" <> image.tag


-- |Extract the integer return code from a ContainerReturnCode
returnCodeToInt :: ContainerReturnCode -> Int
returnCodeToInt (ContainerReturnCode code) = code


-- |Convert Docker container ID to text
containerIdToText :: ContainerId -> Text
containerIdToText (ContainerId id) = id

-- |Extract the content of a volume directive
volumeToText :: Volume -> Text
volumeToText (Volume vol) = vol


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


-- |Constructs HTTP request to fetch logs from Docker
fetchLogs_ :: RequestBuilder -> FetchLogsOptions -> IO ByteString
fetchLogs_ newReq options = do
  let timestampToText t = tshow (round t :: Int)
  let url =
        "/containers/"
        <> containerIdToText options.container
        <> "/logs?stdout=true&stderr=true&since="
        <> timestampToText options.since
        <> "&until="
        <> timestampToText options.until

  res <- HTTP.httpBS $ newReq url
  pure $ HTTP.getResponseBody res


-- |Construct a new shared volume
createVolume_ :: RequestBuilder -> IO Volume
createVolume_ newReq = do
  let body = Aeson.object
            [
              ("Labels", Aeson.object [("turnstile", "0.1.0.0")])
            ]

  let req = newReq "/volumes/create"
            & HTTP.setRequestMethod "POST"
            & HTTP.setRequestBodyJSON body

  let parser = Aeson.withObject "create-volume" $ \o -> do
              name <- o .: "Name"
              pure $ Volume name

  res <- HTTP.httpBS req
  parseResponse res parser


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
      , createVolume = createVolume_ newReq
      , fetchLogs = fetchLogs_ newReq
      , pullImage = pullImage_ newReq
    }

-- |Pull an image programmatically
pullImage_ :: RequestBuilder -> Image -> IO ()
pullImage_ newReq image = do
  let url = "/images/create?tag="
        <> image.tag
        <> "&fromImage="
        <> image.name

  let req = newReq url
            & HTTP.setRequestMethod "POST"

  void $ HTTP.httpBS req

-- |Create a Docker container via the Docker Engine API
createContainer_ :: RequestBuilder -> CreateContainerOptions -> IO ContainerId
createContainer_ newReq options = do
  let image = imageToText options.image
  let bind = volumeToText options.volume <> ":/app"
  let body = Aeson.object
            [
              ("Image", Aeson.toJSON image)
              , ("Tty", Aeson.toJSON True)
              , ("Labels", Aeson.object [("turnstile", "0.1.0.0")])
              , ("Entrypoint", Aeson.toJSON [Aeson.String "/bin/sh", "-c"])

              , ("Cmd", "echo \"$TURNSTILE_BIN\" | /bin/sh")
              , ("Env", Aeson.toJSON ["TURNSTILE_BIN=" <> options.script])

              , ("WorkingDir", "/app")
              , ("HostConfig", Aeson.object [ ("Binds", Aeson.toJSON [bind]) ])
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

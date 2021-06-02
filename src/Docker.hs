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

import qualified SockIO

-- *Types
data CreateContainerOptions
  = CreateContainerOptions
      {
        image :: Image
      }

newtype Image = Image Text
  deriving (Eq, Show)

newtype ContainerReturnCode = ContainerReturnCode Int
  deriving (Eq, Show)

createContainer :: CreateContainerOptions -> IO ()
createContainer options = do
  manager <- SockIO.newManager "/var/run/docker.sock"

  let image = imageToText options.image
  let body = Aeson.object
            [
              ("Image", Aeson.toJSON image)
              , ("Tty", Aeson.toJSON True)
              , ("Labels", Aeson.object [("turnstile", "0.1.0.0")])
              , ("Cmd", "echo testing")
              , ("Entrypoint", Aeson.toJSON [Aeson.String "/bin/sh", "-c"])
            ]

  let req = HTTP.defaultRequest
          & HTTP.setRequestManager manager
          & HTTP.setRequestHost "localhost"
          & HTTP.setRequestPath "/v1.40/containers/create"
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestBodyJSON body

  res <- HTTP.httpBS req

  traceShowIO res

-- *Helpers

-- |Extract the image text from an Image type
imageToText :: Image -> Text
imageToText (Image image) = image

-- |Extract the integer return code from a ContainerReturnCode
returnCodeToInt :: ContainerReturnCode -> Int
returnCodeToInt (ContainerReturnCode code) = code

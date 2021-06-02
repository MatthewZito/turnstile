{-|
Module      : SockIO
Description : Configures socket conn managers
License     : GPL-3
Maintainer  : exbotanical at protonmail
Stability   : experimental
Portability : POSIX

Docker Engine's API exposes a socket. This module configures a Manager
instance for the HTTP lib that allows us to open socket connections
-}
module SockIO where

import RIO

import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.Internal as Client.Internal
import qualified Network.Socket as Sock
import qualified Network.Socket.ByteString as SockB

-- |Socket manager interface for `Network.HTTP`
newManager :: FilePath -> IO Client.Manager
newManager fp =
  Client.newManager $
    Client.defaultManagerSettings
    {
      Client.managerRawConnection = pure makeSocket
    }
  where
    makeSocket _ _ _ = do
      s <- Sock.socket Sock.AF_UNIX Sock.Stream Sock.defaultProtocol
      Sock.connect s (Sock.SockAddrUnix fp)
      Client.Internal.makeConnection
        (SockB.recv s 8096)
        (SockB.sendAll s)
        (Sock.close s)

module Wuss
    ( ClientApp
    , runSecureClient
    , runSecureClientWith
    ) where

import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as BL
import Network.Connection (Connection, ConnectionParams (..), TLSSettings (..),
    connectTo, connectionGetChunk, connectionPut, initConnectionContext)
import Network.Socket (HostName, PortNumber)
import Network.WebSockets (ClientApp, ConnectionOptions, Headers,
    defaultConnectionOptions, runClientWithStream)
import Network.WebSockets.Stream (makeStream)

{- |
    >>> let app _connection = return ()
    >>> runSecureClient "echo.websocket.org" 443 "/" app
-}
runSecureClient
    :: HostName -- ^ Host
    -> PortNumber -- ^ Port
    -> String -- ^ Path
    -> ClientApp a -- ^ Application
    -> IO a
runSecureClient host port path app =
    let options = defaultConnectionOptions
        headers = []
    in  runSecureClientWith host port path options headers app

{- |
    >>> let options = defaultConnectionOptions
    >>> let headers = []
    >>> let app _connection = return ()
    >>> runSecureClientWith "echo.websocket.org" 443 "/" options headers app
-}
runSecureClientWith
    :: HostName -- ^ Host
    -> PortNumber -- ^ Port
    -> String -- ^ Path
    -> ConnectionOptions -- ^ Options
    -> Headers -- ^ Headers
    -> ClientApp a -- ^ Application
    -> IO a
runSecureClientWith host port path options headers app = do
    context <- initConnectionContext
    connection <- connectTo context (connectionParams host port)
    stream <- makeStream (reader connection) (writer connection)
    runClientWithStream stream host path options headers app

connectionParams :: HostName -> PortNumber -> ConnectionParams
connectionParams host port = ConnectionParams
    { connectionHostname = host
    , connectionPort = port
    , connectionUseSecure = Just tlsSettings
    , connectionUseSocks = Nothing
    }

tlsSettings :: TLSSettings
tlsSettings = TLSSettingsSimple
    { settingDisableCertificateValidation = False
    , settingDisableSession = False
    , settingUseServerName = False
    }

reader :: Connection -> IO (Maybe BS.ByteString)
reader connection = fmap Just (connectionGetChunk connection)

writer :: Connection -> Maybe BL.ByteString -> IO ()
writer connection = maybe (return ()) (connectionPut connection . toStrict)

module Wuss
    ( runSecureClient
    , runSecureClientWith
    , runSecureClientWithParams
    , defaultConnectionParams
    , defaultTLSSettings
    ) where

import qualified Data.ByteString as BS
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
    let params = defaultConnectionParams host port
    runSecureClientWithParams host path params options headers app

{- |
    >>> let host = "echo.websocket.org"
    >>> let port = 443
    >>> let path = "/"
    >>> let params = defaultConnectionParams host port
    >>> let options = defaultConnectionOptions
    >>> let headers = []
    >>> let app _connection = return ()
    >>> runSecureClientWithParams host path params options headers app
-}
runSecureClientWithParams
    :: HostName -- ^ Host
    -> String -- ^ Path
    -> ConnectionParams -- ^ Parameters
    -> ConnectionOptions -- ^ Options
    -> Headers -- ^ Headers
    -> ClientApp a -- ^ Application
    -> IO a
runSecureClientWithParams host path params options headers app = do
    context <- initConnectionContext
    connection <- connectTo context params
    stream <- makeStream (reader connection) (writer connection)
    runClientWithStream stream host path options headers app

defaultConnectionParams :: HostName -> PortNumber -> ConnectionParams
defaultConnectionParams host port = ConnectionParams
    { connectionHostname = host
    , connectionPort = port
    , connectionUseSecure = Just defaultTLSSettings
    , connectionUseSocks = Nothing
    }

defaultTLSSettings :: TLSSettings
defaultTLSSettings = TLSSettingsSimple
    { settingDisableCertificateValidation = False
    , settingDisableSession = False
    , settingUseServerName = False
    }

reader :: Connection -> IO (Maybe BS.ByteString)
reader connection = fmap Just (connectionGetChunk connection)

writer :: Connection -> Maybe BL.ByteString -> IO ()
writer connection = maybe (return ()) (connectionPut connection . BL.toStrict)

{- |
    Wuss is a library that lets you easily create secure WebSocket clients over
    the WSS protocol. It is a small addition to
    <https://hackage.haskell.org/package/websockets the websockets package>
    and is adapted from existing solutions by
    <https://gist.github.com/jaspervdj/7198388 @jaspervdj>,
    <https://gist.github.com/mpickering/f1b7ba3190a4bb5884f3 @mpickering>, and
    <https://gist.github.com/elfenlaid/7b5c28065e67e4cf0767 @elfenlaid>.

    == Example

    > import Wuss
    >
    > import Control.Concurrent (forkIO)
    > import Control.Monad (forever, unless, void)
    > import Data.Text (Text, pack)
    > import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData)
    >
    > main :: IO ()
    > main = runSecureClient "echo.websocket.org" 443 "/" ws
    >
    > ws :: ClientApp ()
    > ws connection = do
    >     putStrLn "Connected!"
    >
    >     void . forkIO . forever $ do
    >         message <- receiveData connection
    >         print (message :: Text)
    >
    >     let loop = do
    >             line <- getLine
    >             unless (null line) $ do
    >                 sendTextData connection (pack line)
    >                 loop
    >     loop
    >
    >     sendClose connection (pack "Bye!")
-}
module Wuss
    ( runSecureClient
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
    A secure replacement for 'Network.WebSockets.runClient'.

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
    A secure replacement for 'Network.WebSockets.runClientWith'.

    >>> let options = defaultConnectionOptions
    >>> let headers = []
    >>> let app _connection = return ()
    >>> runSecureClientWith "echo.websocket.org" 443 "/" options headers app

    If you want to run a secure client without certificate validation, use
    'Network.WebSockets.runClientWithStream'. For example:

    > let host = "echo.websocket.org"
    > let port = 443
    > let path = "/"
    > let options = defaultConnectionOptions
    > let headers = []
    > let tlsSettings = TLSSettingsSimple
    >     -- This is the important setting.
    >     { settingDisableCertificateValidation = True
    >     , settingDisableSession = False
    >     , settingUseServerName = False
    >     }
    > let connectionParams = ConnectionParams
    >     { connectionHostname = host
    >     , connectionPort = port
    >     , connectionUseSecure = Just tlsSettings
    >     , connectionUseSocks = Nothing
    >     }
    >
    > context <- initConnectionContext
    > connection <- connectTo context connectionParams
    > stream <- makeStream
    >     (fmap Just (connectionGetChunk connection))
    >     (maybe (return ()) (connectionPut connection . toStrict))
    > runClientWithStream stream host path options headers $ \ connection -> do
    >     -- Do something with the connection.
    >     return ()
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

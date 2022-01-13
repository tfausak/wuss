{-# LANGUAGE NoImplicitPrelude #-}

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

    == Retry

    Note that it is possible for the connection itself or any message to fail and need to be retried.
    Fortunately this can be handled by something like <https://hackage.haskell.org/package/retry the retry package>.
    See <https://github.com/tfausak/wuss/issues/18#issuecomment-990921703 this comment> for an example.
-}
module Wuss
  ( runSecureClient
  , runSecureClientWith
  , Config(..)
  , defaultConfig
  , runSecureClientWithConfig
  ) where

import qualified Control.Applicative as Applicative
import qualified Control.Exception as Exception
import qualified Data.Bool as Bool
import qualified Data.ByteString as StrictBytes
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Maybe as Maybe
import qualified Data.String as String
import qualified Network.Connection as Connection
import qualified Network.Socket as Socket
import qualified Network.WebSockets as WebSockets
import qualified Network.WebSockets.Stream as Stream
import qualified System.IO as IO
import qualified System.IO.Error as IO.Error


{- |
    A secure replacement for 'Network.WebSockets.runClient'.

    >>> let app _connection = return ()
    >>> runSecureClient "echo.websocket.org" 443 "/" app
-}
runSecureClient
  :: Socket.HostName -- ^ Host
  -> Socket.PortNumber -- ^ Port
  -> String.String -- ^ Path
  -> WebSockets.ClientApp a -- ^ Application
  -> IO.IO a
runSecureClient host port path app = do
  let options = WebSockets.defaultConnectionOptions
  runSecureClientWith host port path options [] app


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
  :: Socket.HostName -- ^ Host
  -> Socket.PortNumber -- ^ Port
  -> String.String -- ^ Path
  -> WebSockets.ConnectionOptions -- ^ Options
  -> WebSockets.Headers -- ^ Headers
  -> WebSockets.ClientApp a -- ^ Application
  -> IO.IO a
runSecureClientWith host port path options headers app = do
  let config = defaultConfig
  runSecureClientWithConfig host port path config options headers app


-- | Configures a secure WebSocket connection.
newtype Config = Config
    { connectionGet :: Connection.Connection -> IO.IO StrictBytes.ByteString
    -- ^ How to get bytes from the connection. Typically
    -- 'Connection.connectionGetChunk', but could be something else like
    -- 'Connection.connectionGetLine'.
    }


-- | The default 'Config' value used by 'runSecureClientWith'.
defaultConfig :: Config
defaultConfig = do
  Config { connectionGet = Connection.connectionGetChunk }


-- | Runs a secure WebSockets client with the given 'Config'.
runSecureClientWithConfig
  :: Socket.HostName -- ^ Host
  -> Socket.PortNumber -- ^ Port
  -> String.String -- ^ Path
  -> Config -- ^ Config
  -> WebSockets.ConnectionOptions -- ^ Options
  -> WebSockets.Headers -- ^ Headers
  -> WebSockets.ClientApp a -- ^ Application
  -> IO.IO a
runSecureClientWithConfig host port path config options headers app = do
  context <- Connection.initConnectionContext
  Exception.bracket
    (Connection.connectTo context (connectionParams host port))
    Connection.connectionClose
    (\connection -> do
      stream <- Stream.makeStream
        (reader config connection)
        (writer connection)
      WebSockets.runClientWithStream stream host path options headers app
    )


connectionParams
  :: Socket.HostName -> Socket.PortNumber -> Connection.ConnectionParams
connectionParams host port = do
  Connection.ConnectionParams
    { Connection.connectionHostname = host
    , Connection.connectionPort = port
    , Connection.connectionUseSecure = Maybe.Just tlsSettings
    , Connection.connectionUseSocks = Maybe.Nothing
    }


tlsSettings :: Connection.TLSSettings
tlsSettings = do
  Connection.TLSSettingsSimple
    { Connection.settingDisableCertificateValidation = Bool.False
    , Connection.settingDisableSession = Bool.False
    , Connection.settingUseServerName = Bool.False
    }


reader
  :: Config
  -> Connection.Connection
  -> IO.IO (Maybe.Maybe StrictBytes.ByteString)
reader config connection = IO.Error.catchIOError
  (do
    chunk <- connectionGet config connection
    Applicative.pure (Maybe.Just chunk)
  )
  (\e -> if IO.Error.isEOFError e
    then Applicative.pure Maybe.Nothing
    else Exception.throwIO e
  )


writer
  :: Connection.Connection -> Maybe.Maybe LazyBytes.ByteString -> IO.IO ()
writer connection maybeBytes = do
  case maybeBytes of
    Maybe.Nothing -> do
      Applicative.pure ()
    Maybe.Just bytes -> do
      Connection.connectionPut connection (LazyBytes.toStrict bytes)

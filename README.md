<h1 align="center">
    <a href="http://taylor.fausak.me/wuss/">
        Wuss
    </a>
</h1>

<p align="center">
    Secure WebSocket (WSS) clients in Haskell.
</p>

<p align="center">
    <a href="https://hackage.haskell.org/package/wuss"><img alt="Version" src="https://img.shields.io/hackage/v/wuss.svg?label=version&amp;style=flat-square"></a>
    <a href="https://travis-ci.org/tfausak/wuss"><img alt="Build" src="https://img.shields.io/travis/tfausak/wuss/master.svg?label=build&amp;style=flat-square"></a>
    <a href="http://packdeps.haskellers.com/feed?needle=wuss"><img alt="Dependencies" src="https://img.shields.io/hackage-deps/v/wuss.svg?label=dependencies&amp;style=flat-square"></a>
</p>

<hr>

Wuss is a library that lets you easily create secure WebSocket clients over the
WSS protocol.

-   [Installation](#installation)
-   [Usage](#usage)

## Installation

To add Wuss as a dependency to your package, add it to your Cabal file.

```
build-depends: wuss ==1.*
```

For other use cases, install it with Cabal.

``` sh
$ cabal install 'wuss ==1.*'
```

Wuss uses [Semantic Versioning][]. See [the change log][] for a detailed list
of changes.

## Usage

``` hs
import Wuss

import Control.Concurrent (forkIO)
import Control.Monad (forever, unless, void)
import Data.Text (Text, pack)
import Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData)

main :: IO ()
main = runSecureClient "echo.websocket.org" 443 "/" ws

ws :: ClientApp ()
ws connection = do
    putStrLn "Connected!"

    void . forkIO . forever $ do
        message <- receiveData connection
        print (message :: Text)

    let loop = do
            line <- getLine
            unless (null line) $ do
                sendTextData connection (pack line)
                loop
    loop

    sendClose connection (pack "Bye!")
```

For more information about Wuss, please read [the Haddock documentation][].

[semantic versioning]: http://semver.org/spec/v2.0.0.html
[the change log]: CHANGELOG.md
[the haddock documentation]: https://hackage.haskell.org/package/wuss

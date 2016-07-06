# [Wuss][]

Secure WebSocket (WSS) clients in Haskell.

[![Version badge][]][version]
[![Build badge][]][build]

---

Wuss is a library that lets you easily create secure WebSocket clients over the
WSS protocol. It is a small addition to [the `websockets` package][] and is
adapted from existing solutions by [@jaspervdj][], [@mpickering][], and
[@elfenlaid][].

-   [Installation](#installation)
-   [Usage](#usage)

## Installation

To add Wuss as a dependency to your package, add it to your Cabal file.

```
build-depends: wuss
```

For other use cases, install it with Cabal.

``` sh
$ cabal install wuss
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

[Wuss]: http://taylor.fausak.me/wuss/
[Version badge]: https://www.stackage.org/package/wuss/badge/nightly?label=version
[version]: https://www.stackage.org/package/wuss
[Build badge]: https://travis-ci.org/tfausak/wuss.svg?branch=master
[build]: https://travis-ci.org/tfausak/wuss
[the `websockets` package]: https://hackage.haskell.org/package/websockets
[@jaspervdj]: https://gist.github.com/jaspervdj/7198388
[@mpickering]: https://gist.github.com/mpickering/f1b7ba3190a4bb5884f3
[@elfenlaid]: https://gist.github.com/elfenlaid/7b5c28065e67e4cf0767
[semantic versioning]: http://semver.org/spec/v2.0.0.html
[the change log]: CHANGELOG.md
[the haddock documentation]: https://hackage.haskell.org/package/wuss

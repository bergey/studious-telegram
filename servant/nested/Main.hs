{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Semigroup
import Data.Text (Text)
import           Servant hiding (NotSecure)
import           Servant.Server
import           Network.Wai.Handler.Warp   (defaultSettings, runSettings,
                                             setLogger, setPort)

import qualified Data.Text as T


type MainAPI =
    "static" :> StaticAPI
    :<|> "authenticated" :> AuthenticatedAPI

type StaticAPI =
        "foo" :> Get '[JSON] Text
        :<|> "bar" :> Get '[JSON] Text

type AuthenticatedAPI = Capture "echo" Text :> (
        "baz" :> Get '[JSON] Text
        :<|> "quux" :> Get '[JSON] Text
        :<|> "wibble" :> Get '[JSON] Text
        )

mainServer :: Server MainAPI
mainServer = static
    :<|> authenticated

static = foo :<|> bar

authenticated e = baz e :<|> quux e :<|> wibble e

foo = return "foo"
bar = return "bar"
baz e = return ("baz = " <> e)
quux e = return ("quux = " <> e)
wibble e = return ("wibble = " <> e)

main :: IO ()
main = runSettings defaultSettings $
    serveWithContext (Proxy :: Proxy MainAPI) EmptyContext mainServer

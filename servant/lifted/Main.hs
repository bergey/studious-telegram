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
    "static" :> "foo" :> Get '[JSON] Text
    :<|> "static" :> "bar" :> Get '[JSON] Text
    :<|> "authenticated" :> Capture "echo" Text :> (
        "baz" :> Get '[JSON] Text
        :<|> "quux" :> Get '[JSON] Text
        :<|> "wibble" :> Get '[JSON] Text
        )

mainServer :: Server MainAPI
mainServer = foo
    :<|> bar
    :<|> authenticated
  where authenticated e =
            baz e
            :<|> quux e
            :<|> wibble e

foo = return "foo"
bar = return "bar"
baz e = return ("baz = " <> e)
quux e = return ("quux = " <> e)
wibble e = return ("wibble = " <> e)

main :: IO ()
main = runSettings defaultSettings $
    serveWithContext (Proxy :: Proxy MainAPI) EmptyContext mainServer

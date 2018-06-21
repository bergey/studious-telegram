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
    "static" :> "1" :> Get '[JSON] Text
    :<|> "static" :> "2" :> Get '[JSON] Text
    :<|> "static" :> "3" :> Get '[JSON] Text
    :<|> "static" :> "4" :> Get '[JSON] Text

mainServer :: Server MainAPI
mainServer = foo
    :<|> foo
    :<|> foo
    :<|> foo

foo = return "foo"

main :: IO ()
main = runSettings defaultSettings $
    serveWithContext (Proxy :: Proxy MainAPI) EmptyContext mainServer

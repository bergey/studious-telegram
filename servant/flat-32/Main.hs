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
    :<|> "static" :> "5" :> Get '[JSON] Text
    :<|> "static" :> "6" :> Get '[JSON] Text
    :<|> "static" :> "7" :> Get '[JSON] Text
    :<|> "static" :> "8" :> Get '[JSON] Text
    :<|> "static" :> "9" :> Get '[JSON] Text
    :<|> "static" :> "10" :> Get '[JSON] Text
    :<|> "static" :> "11" :> Get '[JSON] Text
    :<|> "static" :> "12" :> Get '[JSON] Text
    :<|> "static" :> "13" :> Get '[JSON] Text
    :<|> "static" :> "14" :> Get '[JSON] Text
    :<|> "static" :> "15" :> Get '[JSON] Text
    :<|> "static" :> "16" :> Get '[JSON] Text
    :<|> "static" :> "17" :> Get '[JSON] Text
    :<|> "static" :> "18" :> Get '[JSON] Text
    :<|> "static" :> "19" :> Get '[JSON] Text
    :<|> "static" :> "20" :> Get '[JSON] Text
    :<|> "static" :> "21" :> Get '[JSON] Text
    :<|> "static" :> "22" :> Get '[JSON] Text
    :<|> "static" :> "23" :> Get '[JSON] Text
    :<|> "static" :> "24" :> Get '[JSON] Text
    :<|> "static" :> "25" :> Get '[JSON] Text
    :<|> "static" :> "26" :> Get '[JSON] Text
    :<|> "static" :> "27" :> Get '[JSON] Text
    :<|> "static" :> "28" :> Get '[JSON] Text
    :<|> "static" :> "29" :> Get '[JSON] Text
    :<|> "static" :> "30" :> Get '[JSON] Text
    :<|> "static" :> "31" :> Get '[JSON] Text
    :<|> "static" :> "32" :> Get '[JSON] Text

mainServer :: Server MainAPI
mainServer = foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo

foo = return "foo"

main :: IO ()
main = runSettings defaultSettings $
    serveWithContext (Proxy :: Proxy MainAPI) EmptyContext mainServer

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import One
import Two
import Three
import Four
import Five
import Six
import Seven
import Eight

import Data.Text (Text)
import           Servant hiding (NotSecure)
import           Servant.Server
import           Network.Wai.Handler.Warp   (defaultSettings, runSettings,
                                             setLogger, setPort)

type MainAPI =
    "1" :> One
    :<|> "2" :> Two
    :<|> "3" :> Three
    :<|> "4" :> Four
    :<|> "5" :> Five
    :<|> "6" :> Six
    :<|> "7" :> Seven
    :<|> "8" :> Eight

mainServer :: Server MainAPI
mainServer = one :<|> two :<|> three :<|> four :<|> five :<|> six :<|> seven :<|> eight

main :: IO ()
main = runSettings defaultSettings $
    serveWithContext (Proxy :: Proxy MainAPI) EmptyContext mainServer

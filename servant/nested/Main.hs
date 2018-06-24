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
    "1" :> One
    :<|> "2" :> Two
    :<|> "3" :> Three
    :<|> "4" :> Four
    :<|> "5" :> Five
    :<|> "6" :> Six
    :<|> "7" :> Seven
    :<|> "8" :> Eight

type One =
    "1" :> Get '[JSON] Text
    :<|> "2" :> Get '[JSON] Text
    :<|> "3" :> Get '[JSON] Text
    :<|> "4" :> Get '[JSON] Text
    :<|> "5" :> Get '[JSON] Text
    :<|> "6" :> Get '[JSON] Text
    :<|> "7" :> Get '[JSON] Text
    :<|> "8" :> Get '[JSON] Text

type Two =
    "1" :> Get '[JSON] Text
    :<|> "2" :> Get '[JSON] Text
    :<|> "3" :> Get '[JSON] Text
    :<|> "4" :> Get '[JSON] Text
    :<|> "5" :> Get '[JSON] Text
    :<|> "6" :> Get '[JSON] Text
    :<|> "7" :> Get '[JSON] Text
    :<|> "8" :> Get '[JSON] Text

type Three =
    "1" :> Get '[JSON] Text
    :<|> "2" :> Get '[JSON] Text
    :<|> "3" :> Get '[JSON] Text
    :<|> "4" :> Get '[JSON] Text
    :<|> "5" :> Get '[JSON] Text
    :<|> "6" :> Get '[JSON] Text
    :<|> "7" :> Get '[JSON] Text
    :<|> "8" :> Get '[JSON] Text

type Four =
    "1" :> Get '[JSON] Text
    :<|> "2" :> Get '[JSON] Text
    :<|> "3" :> Get '[JSON] Text
    :<|> "4" :> Get '[JSON] Text
    :<|> "5" :> Get '[JSON] Text
    :<|> "6" :> Get '[JSON] Text
    :<|> "7" :> Get '[JSON] Text
    :<|> "8" :> Get '[JSON] Text

type Five =
    "1" :> Get '[JSON] Text
    :<|> "2" :> Get '[JSON] Text
    :<|> "3" :> Get '[JSON] Text
    :<|> "4" :> Get '[JSON] Text
    :<|> "5" :> Get '[JSON] Text
    :<|> "6" :> Get '[JSON] Text
    :<|> "7" :> Get '[JSON] Text
    :<|> "8" :> Get '[JSON] Text

type Six =
    "1" :> Get '[JSON] Text
    :<|> "2" :> Get '[JSON] Text
    :<|> "3" :> Get '[JSON] Text
    :<|> "4" :> Get '[JSON] Text
    :<|> "5" :> Get '[JSON] Text
    :<|> "6" :> Get '[JSON] Text
    :<|> "7" :> Get '[JSON] Text
    :<|> "8" :> Get '[JSON] Text

type Seven =
    "1" :> Get '[JSON] Text
    :<|> "2" :> Get '[JSON] Text
    :<|> "3" :> Get '[JSON] Text
    :<|> "4" :> Get '[JSON] Text
    :<|> "5" :> Get '[JSON] Text
    :<|> "6" :> Get '[JSON] Text
    :<|> "7" :> Get '[JSON] Text
    :<|> "8" :> Get '[JSON] Text

type Eight =
    "1" :> Get '[JSON] Text
    :<|> "2" :> Get '[JSON] Text
    :<|> "3" :> Get '[JSON] Text
    :<|> "4" :> Get '[JSON] Text
    :<|> "5" :> Get '[JSON] Text
    :<|> "6" :> Get '[JSON] Text
    :<|> "7" :> Get '[JSON] Text
    :<|> "8" :> Get '[JSON] Text

mainServer :: Server MainAPI
mainServer = one :<|> two :<|> three :<|> four :<|> five :<|> six :<|> seven :<|> eight

one :: Server One
one = foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo

two :: Server Two
two = foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo

three :: Server Three
three = foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo

four :: Server Four
four = foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo

five :: Server Five
five = foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo

six :: Server Six
six = foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo

seven :: Server Seven
seven = foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo

eight :: Server Eight
eight = foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo

foo :: Monad m => m Text
foo = return "foo"

main :: IO ()
main = runSettings defaultSettings $
    serveWithContext (Proxy :: Proxy MainAPI) EmptyContext mainServer

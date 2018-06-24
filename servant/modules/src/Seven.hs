{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

 module Seven where

import Data.Text (Text)
import           Servant hiding (NotSecure)
import           Servant.Server

type Seven =
    "1" :> Get '[JSON] Text
    :<|> "2" :> Get '[JSON] Text
    :<|> "3" :> Get '[JSON] Text
    :<|> "4" :> Get '[JSON] Text
    :<|> "5" :> Get '[JSON] Text
    :<|> "6" :> Get '[JSON] Text
    :<|> "7" :> Get '[JSON] Text
    :<|> "8" :> Get '[JSON] Text

seven :: Server Seven
seven = foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo

foo :: Monad m => m Text
foo = return "foo"

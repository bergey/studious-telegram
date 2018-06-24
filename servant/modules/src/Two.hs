{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Two where

import Data.Text (Text)
import           Servant hiding (NotSecure)
import           Servant.Server

type Two =
    "1" :> Get '[JSON] Text
    :<|> "2" :> Get '[JSON] Text
    :<|> "3" :> Get '[JSON] Text
    :<|> "4" :> Get '[JSON] Text
    :<|> "5" :> Get '[JSON] Text
    :<|> "6" :> Get '[JSON] Text
    :<|> "7" :> Get '[JSON] Text
    :<|> "8" :> Get '[JSON] Text

two :: Server Two
two = foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo

foo :: Monad m => m Text
foo = return "foo"

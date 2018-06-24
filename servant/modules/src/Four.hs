{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Four where

import Data.Text (Text)
import           Servant hiding (NotSecure)
import           Servant.Server

type Four =
    "1" :> Get '[JSON] Text
    :<|> "2" :> Get '[JSON] Text
    :<|> "3" :> Get '[JSON] Text
    :<|> "4" :> Get '[JSON] Text

four :: Server Four
four = foo :<|> foo :<|> foo :<|> foo

foo :: Monad m => m Text
foo = return "foo"

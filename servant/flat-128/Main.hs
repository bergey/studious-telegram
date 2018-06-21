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
    :<|> "static" :> "33" :> Get '[JSON] Text
    :<|> "static" :> "34" :> Get '[JSON] Text
    :<|> "static" :> "35" :> Get '[JSON] Text
    :<|> "static" :> "36" :> Get '[JSON] Text
    :<|> "static" :> "37" :> Get '[JSON] Text
    :<|> "static" :> "38" :> Get '[JSON] Text
    :<|> "static" :> "39" :> Get '[JSON] Text
    :<|> "static" :> "40" :> Get '[JSON] Text
    :<|> "static" :> "41" :> Get '[JSON] Text
    :<|> "static" :> "42" :> Get '[JSON] Text
    :<|> "static" :> "43" :> Get '[JSON] Text
    :<|> "static" :> "44" :> Get '[JSON] Text
    :<|> "static" :> "45" :> Get '[JSON] Text
    :<|> "static" :> "46" :> Get '[JSON] Text
    :<|> "static" :> "47" :> Get '[JSON] Text
    :<|> "static" :> "48" :> Get '[JSON] Text
    :<|> "static" :> "49" :> Get '[JSON] Text
    :<|> "static" :> "50" :> Get '[JSON] Text
    :<|> "static" :> "51" :> Get '[JSON] Text
    :<|> "static" :> "52" :> Get '[JSON] Text
    :<|> "static" :> "53" :> Get '[JSON] Text
    :<|> "static" :> "54" :> Get '[JSON] Text
    :<|> "static" :> "55" :> Get '[JSON] Text
    :<|> "static" :> "56" :> Get '[JSON] Text
    :<|> "static" :> "57" :> Get '[JSON] Text
    :<|> "static" :> "58" :> Get '[JSON] Text
    :<|> "static" :> "59" :> Get '[JSON] Text
    :<|> "static" :> "60" :> Get '[JSON] Text
    :<|> "static" :> "61" :> Get '[JSON] Text
    :<|> "static" :> "62" :> Get '[JSON] Text
    :<|> "static" :> "63" :> Get '[JSON] Text
    :<|> "static" :> "64" :> Get '[JSON] Text
    :<|> "static" :> "65" :> Get '[JSON] Text
    :<|> "static" :> "66" :> Get '[JSON] Text
    :<|> "static" :> "67" :> Get '[JSON] Text
    :<|> "static" :> "68" :> Get '[JSON] Text
    :<|> "static" :> "69" :> Get '[JSON] Text
    :<|> "static" :> "70" :> Get '[JSON] Text
    :<|> "static" :> "71" :> Get '[JSON] Text
    :<|> "static" :> "72" :> Get '[JSON] Text
    :<|> "static" :> "73" :> Get '[JSON] Text
    :<|> "static" :> "74" :> Get '[JSON] Text
    :<|> "static" :> "75" :> Get '[JSON] Text
    :<|> "static" :> "76" :> Get '[JSON] Text
    :<|> "static" :> "77" :> Get '[JSON] Text
    :<|> "static" :> "78" :> Get '[JSON] Text
    :<|> "static" :> "79" :> Get '[JSON] Text
    :<|> "static" :> "80" :> Get '[JSON] Text
    :<|> "static" :> "81" :> Get '[JSON] Text
    :<|> "static" :> "82" :> Get '[JSON] Text
    :<|> "static" :> "83" :> Get '[JSON] Text
    :<|> "static" :> "84" :> Get '[JSON] Text
    :<|> "static" :> "85" :> Get '[JSON] Text
    :<|> "static" :> "86" :> Get '[JSON] Text
    :<|> "static" :> "87" :> Get '[JSON] Text
    :<|> "static" :> "88" :> Get '[JSON] Text
    :<|> "static" :> "89" :> Get '[JSON] Text
    :<|> "static" :> "90" :> Get '[JSON] Text
    :<|> "static" :> "91" :> Get '[JSON] Text
    :<|> "static" :> "92" :> Get '[JSON] Text
    :<|> "static" :> "93" :> Get '[JSON] Text
    :<|> "static" :> "94" :> Get '[JSON] Text
    :<|> "static" :> "95" :> Get '[JSON] Text
    :<|> "static" :> "96" :> Get '[JSON] Text
    :<|> "static" :> "97" :> Get '[JSON] Text
    :<|> "static" :> "98" :> Get '[JSON] Text
    :<|> "static" :> "99" :> Get '[JSON] Text
    :<|> "static" :> "100" :> Get '[JSON] Text
    :<|> "static" :> "101" :> Get '[JSON] Text
    :<|> "static" :> "102" :> Get '[JSON] Text
    :<|> "static" :> "103" :> Get '[JSON] Text
    :<|> "static" :> "104" :> Get '[JSON] Text
    :<|> "static" :> "105" :> Get '[JSON] Text
    :<|> "static" :> "106" :> Get '[JSON] Text
    :<|> "static" :> "107" :> Get '[JSON] Text
    :<|> "static" :> "108" :> Get '[JSON] Text
    :<|> "static" :> "109" :> Get '[JSON] Text
    :<|> "static" :> "110" :> Get '[JSON] Text
    :<|> "static" :> "111" :> Get '[JSON] Text
    :<|> "static" :> "112" :> Get '[JSON] Text
    :<|> "static" :> "113" :> Get '[JSON] Text
    :<|> "static" :> "114" :> Get '[JSON] Text
    :<|> "static" :> "115" :> Get '[JSON] Text
    :<|> "static" :> "116" :> Get '[JSON] Text
    :<|> "static" :> "117" :> Get '[JSON] Text
    :<|> "static" :> "118" :> Get '[JSON] Text
    :<|> "static" :> "119" :> Get '[JSON] Text
    :<|> "static" :> "120" :> Get '[JSON] Text
    :<|> "static" :> "121" :> Get '[JSON] Text
    :<|> "static" :> "122" :> Get '[JSON] Text
    :<|> "static" :> "123" :> Get '[JSON] Text
    :<|> "static" :> "124" :> Get '[JSON] Text
    :<|> "static" :> "125" :> Get '[JSON] Text
    :<|> "static" :> "126" :> Get '[JSON] Text
    :<|> "static" :> "127" :> Get '[JSON] Text
    :<|> "static" :> "128" :> Get '[JSON] Text

mainServer :: Server MainAPI
mainServer = foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
    :<|> foo :<|> foo :<|> foo :<|> foo
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

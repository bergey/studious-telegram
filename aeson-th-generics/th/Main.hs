{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Aeson
import Data.Aeson.TH
import Data.Semigroup
import Data.Text (Text)

import qualified Data.Text as T
-- import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.Char8 as BSL

data Benchmark = Benchmark
    { benchmarkName :: Text
    , benchmarkCategory :: Text
    , benchmarkVariant :: Text
    } deriving (Show, Read, Eq)
$(deriveJSON defaultOptions ''Benchmark)

benchmark :: Benchmark
benchmark = Benchmark "lens-th" "lens" "th"

main :: IO ()
main = do
    BSL.putStrLn $ "running: " <> encode benchmark

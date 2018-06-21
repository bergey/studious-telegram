{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)

import qualified Data.Text as T
import qualified Data.Text.IO as T

data Benchmark = Benchmark
    { benchmarkName :: Text
    , benchmarkCategory :: Text
    , benchmarkVariant :: Text
    } deriving (Show, Read, Eq)

benchmark :: Benchmark
benchmark = Benchmark "aeson-baseline" "aeson" "baseline"

main :: IO ()
main = do
    T.putStrLn $ "running: {\"benchmarkName\":\"aeson-baseline\",\"benchmarkCategory\":\"aeson\",\"benchmarkVariant\":\"baseline\"}"

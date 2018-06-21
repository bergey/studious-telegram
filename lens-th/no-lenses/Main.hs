{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Semigroup
import Data.Text (Text)

import qualified Data.Text as T
import qualified Data.Text.IO as T

data Benchmark = Benchmark
    { benchmarkName :: Text
    , benchmarkCategory :: Text
    , benchmarkVariant :: Text
    } deriving (Show, Read, Eq)

benchmark :: Benchmark
benchmark = Benchmark "lens-th" "lens" "th"

main :: IO ()
main = do
    T.putStrLn $ "running: " <> benchmarkName benchmark

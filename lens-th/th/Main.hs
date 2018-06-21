{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Semigroup
import Data.Text (Text)

import qualified Data.Text as T
import qualified Data.Text.IO as T

data Benchmark = Benchmark
    { benchmarkName :: Text
    , benchmarkCategory :: Text
    , benchmarkVariant :: Text
    } deriving (Show, Read, Eq)
makeFields ''Benchmark

benchmark :: Benchmark
benchmark = Benchmark "lens-th" "lens" "th"

main :: IO ()
main = do
    T.putStrLn $ "running: " <> benchmark ^. name

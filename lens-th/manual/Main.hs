{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Data.Semigroup
import Data.Text (Text)

import qualified Data.Text as T
import qualified Data.Text.IO as T

data Benchmark = Benchmark
    { benchmarkName :: Text
    , benchmarkCategory :: Text
    , benchmarkCase :: Text
    } deriving (Show, Read, Eq)

-- This is the code output from th/Main.hs with -ddump-splices
class HasCategory s a | s -> a where
  category :: Lens' s a
instance HasCategory Benchmark Text where
  {-# INLINE category #-}
  category f_aab4 (Benchmark x1_aab5 x2_aab6 x3_aab7)
    = (fmap (\ y1_aab8 -> ((Benchmark x1_aab5) y1_aab8) x3_aab7))
        (f_aab4 x2_aab6)
class HasName s a | s -> a where
  name :: Lens' s a
instance HasName Benchmark Text where
  {-# INLINE name #-}
  name f_aab9 (Benchmark x1_aaba x2_aabb x3_aabc)
    = (fmap (\ y1_aabd -> ((Benchmark y1_aabd) x2_aabb) x3_aabc))
        (f_aab9 x1_aaba)
class HasVariant s a | s -> a where
  variant :: Lens' s a
instance HasVariant Benchmark Text where
      {-# INLINE variant #-}
      variant f_aabe (Benchmark x1_aabf x2_aabg x3_aabh)
        = (fmap (\ y1_aabi -> ((Benchmark x1_aabf) x2_aabg) y1_aabi))
            (f_aabe x3_aabh)

benchmark :: Benchmark
benchmark = Benchmark "lens-manual" "lens" "manual"

main :: IO ()
main = do
    T.putStrLn $ "running: " <> benchmark ^. name

module Main (main) where

import qualified Benchmark
import Criterion.Main

main :: IO ()
main = defaultMain [Benchmark.benchmark]

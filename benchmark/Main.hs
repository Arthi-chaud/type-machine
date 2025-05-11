module Main (main) where

import qualified Build
import Criterion.Main
import qualified Traverse

main :: IO ()
main = defaultMain [Build.benchmark, Traverse.benchmark]

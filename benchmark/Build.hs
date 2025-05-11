module Build (benchmark) where

import Criterion.Main

benchmark :: Benchmark
benchmark = bgroup "build" []

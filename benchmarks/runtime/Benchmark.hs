{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Benchmark (benchmarks) where

import Control.Lens ((^.))
import Criterion.Main
import Data.Functor
import Gen
import SuperRecord (get)

$(genWithFieldCount 100)

benchmarks :: [Benchmark]
benchmarks = []

-- benchmarks :: [Benchmark]
-- benchmarks = [build, traversals]
--
-- data Case = TM | SuperRecord | Extensible
--
-- buildRecord :: Case -> Int -> ()
-- buildRecord c n = flip seq () $ case n of
--     10 -> case c of
--         TM -> field10_9 mkRecordTM10
--         SuperRecord -> get #fSR10_9 mkSuperRRecord10
--         Extensible -> mkExtensibleRecord10 ^. #fE10_9
--     20 -> case c of
--         TM -> field20_19 mkRecordTM20
--         SuperRecord -> get #fSR20_19 mkSuperRRecord20
--         Extensible -> mkExtensibleRecord20 ^. #fE20_19
--     30 -> case c of
--         TM -> field30_29 mkRecordTM30
--         SuperRecord -> get #fSR30_29 mkSuperRRecord30
--         Extensible -> mkExtensibleRecord30 ^. #fE30_29
--     _ -> error "unhandled n"
--
-- build :: Benchmark
-- build =
--     bgroup
--         "build"
--         $ depths <&> \n ->
--             bgroup
--                 (show n)
--                 ( cases <&> \(bname, case_) ->
--                     bench bname $
--                         nf
--                             (uncurry buildRecord)
--                             (case_, n)
--                 )
--   where
--     depths = [10, 20, 30]
--     cases = [("tm", TM), ("superrecord", SuperRecord), ("extensible", Extensible)]
--
-- traversals :: Benchmark
-- traversals =
--     bgroup
--         "traverse"
--         [ bgroup
--             "10"
--             [ bench "tm" $ nf sumRecordTM10 mkRecordTM10
--             , bench "superrecord" $ nf sumSuperRRecord10 mkSuperRRecord10
--             , bench "extensible" $ nf sumExtensibleRecord10 mkExtensibleRecord10
--             ]
--         , bgroup
--             "20"
--             [ bench "tm" $ nf sumRecordTM20 mkRecordTM20
--             , bench "superrecord" $ nf sumSuperRRecord20 mkSuperRRecord20
--             , bench "extensible" $ nf sumExtensibleRecord20 mkExtensibleRecord20
--             ]
--         , bgroup
--             "30"
--             [ bench "tm" $ nf sumRecordTM30 mkRecordTM30
--             , bench "superrecord" $ nf sumSuperRRecord30 mkSuperRRecord30
--             , bench "extensible" $ nf sumExtensibleRecord30 mkExtensibleRecord30
--             ]
--         ]

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
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Benchmark (benchmarks) where

import Control.Lens ((^.))
import Criterion.Main
import Data.Functor
import Gen
import SuperRecord (get)

$(genWithFieldCount 2)
$(genWithFieldCount 4)
$(genWithFieldCount 5)
$(genWithFieldCount 10)
$(genWithFieldCount 15)
$(genWithFieldCount 20)

benchmarks :: [Benchmark]
benchmarks = [build, traversals]

data Case = TM | SuperRecord | Extensible

buildRecord :: Case -> Int -> ()
buildRecord c n = flip seq () $ case n of
    2 -> case c of
        TM -> field2_1 mkRecordTM2
        SuperRecord -> get #fSR2_1 mkSuperRRecord2
        Extensible -> mkExtensibleRecord2 ^. #fE2_1
    4 -> case c of
        TM -> field4_1 mkRecordTM4
        SuperRecord -> get #fSR4_3 mkSuperRRecord4
        Extensible -> mkExtensibleRecord4 ^. #fE4_3
    5 -> case c of
        TM -> field5_1 mkRecordTM5
        SuperRecord -> get #fSR5_4 mkSuperRRecord5
        Extensible -> mkExtensibleRecord5 ^. #fE5_4
    10 -> case c of
        TM -> field10_9 mkRecordTM10
        SuperRecord -> get #fSR10_9 mkSuperRRecord10
        Extensible -> mkExtensibleRecord10 ^. #fE10_9
    15 -> case c of
        TM -> field15_14 mkRecordTM15
        SuperRecord -> get #fSR15_14 mkSuperRRecord15
        Extensible -> mkExtensibleRecord15 ^. #fE15_14
    20 -> case c of
        TM -> field20_19 mkRecordTM20
        SuperRecord -> get #fSR20_19 mkSuperRRecord20
        Extensible -> mkExtensibleRecord20 ^. #fE20_19
    _ -> error "unhandled n"

build :: Benchmark
build =
    bgroup
        "build"
        $ depths <&> \n ->
            bgroup
                (show n)
                ( cases <&> \(bname, case_) ->
                    bench bname $
                        nf
                            (uncurry buildRecord)
                            (case_, n)
                )
  where
    depths = [2, 4, 5, 10, 20]
    cases = [("tm", TM), ("superrecord", SuperRecord), ("extensible", Extensible)]

traversals :: Benchmark
traversals =
    bgroup
        "traverse"
        [ bgroup
            "2"
            [ bench "tm" $ nf sumRecordTM2 mkRecordTM2
            , bench "superrecord" $ nf sumSuperRRecord2 mkSuperRRecord2
            , bench "extensible" $ nf sumExtensibleRecord2 mkExtensibleRecord2
            ]
        , bgroup
            "4"
            [ bench "tm" $ nf sumRecordTM4 mkRecordTM4
            , bench "superrecord" $ nf sumSuperRRecord4 mkSuperRRecord4
            , bench "extensible" $ nf sumExtensibleRecord4 mkExtensibleRecord4
            ]
        , bgroup
            "5"
            [ bench "tm" $ nf sumRecordTM5 mkRecordTM5
            , bench "superrecord" $ nf sumSuperRRecord5 mkSuperRRecord5
            , bench "extensible" $ nf sumExtensibleRecord5 mkExtensibleRecord5
            ]
        , bgroup
            "10"
            [ bench "tm" $ nf sumRecordTM10 mkRecordTM10
            , bench "superrecord" $ nf sumSuperRRecord10 mkSuperRRecord10
            , bench "extensible" $ nf sumExtensibleRecord10 mkExtensibleRecord10
            ]
        , bgroup
            "15"
            [ bench "tm" $ nf sumRecordTM15 mkRecordTM15
            , bench "superrecord" $ nf sumSuperRRecord15 mkSuperRRecord15
            , bench "extensible" $ nf sumExtensibleRecord15 mkExtensibleRecord15
            ]
        , bgroup
            "20"
            [ bench "tm" $ nf sumRecordTM20 mkRecordTM20
            , bench "superrecord" $ nf sumSuperRRecord20 mkSuperRRecord20
            , bench "extensible" $ nf sumExtensibleRecord20 mkExtensibleRecord20
            ]
        ]

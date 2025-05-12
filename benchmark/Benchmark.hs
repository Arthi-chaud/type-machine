{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Benchmark (benchmark) where

import Control.Lens ((^.))
import Criterion.Main
import Gen

$(genTMRecord 20)
$(genSuperrecord 20)
$(genExtensible 20)

$(genTMRecord 30)
$(genSuperrecord 30)
$(genExtensible 30)

$(genTMRecord 40)
$(genSuperrecord 40)
$(genExtensible 40)

benchmark :: Benchmark
benchmark =
    bgroup
        "traverse"
        [ bgroup
            "20"
            [ bench "tm" $ nf (\() -> sumRecordTM20 mkRecordTM20) ()
            , bench "superrecord" $ nf (\() -> sumSuperRRecord20 mkSuperRRecord20) ()
            , bench "extensible" $ nf (\() -> sumExtensibleRecord20 mkExtensibleRecord20) ()
            ]
        , bgroup
            "30"
            [ bench "tm" $ nf (\() -> sumRecordTM30 mkRecordTM30) ()
            , bench "superrecord" $ nf (\() -> sumSuperRRecord30 mkSuperRRecord30) ()
            , bench "extensible" $ nf (\() -> sumExtensibleRecord30 mkExtensibleRecord30) ()
            ]
        , bgroup
            "40"
            [ bench "tm" $ nf (\() -> sumRecordTM40 mkRecordTM40) ()
            , bench "superrecord" $ nf (\() -> sumSuperRRecord40 mkSuperRRecord40) ()
            , bench "extensible" $ nf (\() -> sumExtensibleRecord40 mkExtensibleRecord40) ()
            ]
        ]

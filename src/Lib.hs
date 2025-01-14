{-# LANGUAGE DuplicateRecordFields #-}

module Lib where

import TH

data User = User
    { id :: Int
    , name :: String
    , otherProp :: Maybe Char
    }

type_ "UserWithoutName" "User"

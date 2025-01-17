{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}

-- | Note: As for now, tests are just here to verify that simple code using TM compiles
module Main (main) where

import TypeMachine.TH
import TypeMachine.TypeFunction
import Prelude hiding (id)

data User = User
    { id :: Int
    , name :: String
    , otherProp :: Maybe Char
    }

-- class IsUser a where
--     getName :: String
--     getId :: Int
--     otherProp :: MaybeChar
$(defineIs ''User)

$(type_ "UserWithoutId" ''User (remove "id"))

-- Does not work since id is a required field or user

-- $(deriveIs ''User ''UserWithoutId)

$(type_ "UserWithoutOtherProp" ''User (remove "otherProp"))

$(type_ "UserWithRequiredOtherProp" ''User (require "otherProp"))

$(type_ "X" ''User (remove "idonotexist"))

newtype MyNewType = MyNewType {x :: Int}

$(type_ "MyNewTypeEmpty" ''MyNewType (remove "x"))

$(deriveIs ''User ''UserWithoutOtherProp)
main :: IO ()
main = putStrLn "Compilation successful :)"

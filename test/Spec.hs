{-# LANGUAGE DuplicateRecordFields #-}

-- | Note: As for now, tests are just here to verify that simple code using TM compiles
module Main (main) where

import TypeMachine.TH
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

$(type_ "UserWithoutName" ''User)

-- instance IsUser UserWithoutName where
--     getId = getField @"id"

-- $(deriveIs ''User ''UserWithoutName)
main :: IO ()
main = putStrLn "Compilation successful :)"

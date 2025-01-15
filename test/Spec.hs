{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}

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

$(type_ "UserWithoutId" ''User (removeField "id"))

-- Does not work since id is a required field or user

-- $(deriveIs ''User ''UserWithoutId)

$(type_ "UserWithoutOtherProp" ''User (removeField "otherProp"))

$(deriveIs ''User ''UserWithoutOtherProp)
main :: IO ()
main = putStrLn "Compilation successful :)"

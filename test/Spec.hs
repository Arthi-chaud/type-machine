{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}

-- | Note: As for now, tests are just here to verify that simple code using TM compiles
module Main (main) where

import TypeMachine.TH
import TypeMachine.TM
import TypeMachine.TM.Syntax
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

$(type_ "UserWithoutId" (remove "id" <:> toType ''User))

-- Does not work since id is a required field or user

-- $(deriveIs ''User ''UserWithoutId)

$(type_ "UserWithoutOtherProp" (remove "otherProp" <:> toType ''User))

$(deriveIs ''User ''UserWithoutOtherProp)

$(type_ "UserWithRequiredOtherProp" (require "otherProp" <:> toType ''User))

$(type_ "X" (remove "idonotexist" <:> toType ''User))

newtype MyNewType = MyNewType {x :: Int}

$(type_ "MyNewTypeEmpty" (remove "x" <:> toType ''MyNewType))

$(type_ "UserId" (pick ["id"] <:> toType ''User))

$(type_ "UserWithWarning" (pick ["x"] <:> toType ''User))

-- $(type_ "CheckApplicationFixity" (flip pick <:> toType ''User $ ["id"]))

-- Has all the fields except id
$(type_ "UserWithoutId2" (intersection <:> toType ''UserWithoutId <:> toType ''User))

$(type_ "PartialUser" (partial <:> toType ''User))

main :: IO ()
main = putStrLn "Compilation successful :)"

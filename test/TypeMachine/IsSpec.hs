{-# LANGUAGE DuplicateRecordFields #-}

module TypeMachine.IsSpec (spec) where

import Test.Hspec
import TypeMachine

data User = User
    { id :: Int
    , name :: String
    , optProp :: Maybe ()
    }
    deriving (Show, Eq)

$(defineIs ''User)
$(type_ "UserWithoutOptProp" (omit ["optProp"] <::> ''User))
$(deriveIs ''User ''UserWithoutOptProp)

spec :: Spec
spec =
    describe
        "'Is' typeclass"
        $ do
            describe "get fields using typeclass" $
                do
                    it "original object" $ do
                        let og = User 1 "A" (Just ())
                        getId og `shouldBe` 1
                        getName og `shouldBe` "A"
                        getOptProp og `shouldBe` Just ()

                    it "secondary object" $ do
                        let og = UserWithoutOptProp 1 "A"
                        getId og `shouldBe` 1
                        getName og `shouldBe` "A"
                        getOptProp og `shouldBe` Nothing
            describe "'to' function" $
                do
                    let og = User 1 "A" (Just ())
                    it "original object (identity)" $ do
                        toUser og `shouldBe` og
                    it "secondary object" $ do
                        let obj = UserWithoutOptProp 1 "A"
                        toUser obj `shouldBe` og{optProp = Nothing}

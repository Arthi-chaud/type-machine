module TestTypeMachine.Functions (specs) where

import Control.Monad
import qualified Data.Map.Strict as Map
import Language.Haskell.TH (Bang (Bang), SourceStrictness (NoSourceStrictness), SourceUnpackedness (NoSourceUnpackedness), mkName, nameBase, runQ)
import Language.Haskell.TH.Syntax (Type (..))
import Test.Hspec
import TypeMachine
import TypeMachine.TM (execTM)
import TypeMachine.Type (Type (..))

data User = User
    { id :: Int
    , name :: String
    , otherProp :: Maybe Char
    }

specs :: Spec
specs =
    describe
        "Functions"
        $ do
            let bang = Bang NoSourceUnpackedness NoSourceStrictness
                idKey = "id"
                nameKey = "name"
                otherPropKey = "otherProp"
                userIdType = (bang, ConT $ mkName "Int")
                userNameType = (bang, ConT $ mkName "String")
                userOtherPropRequiredType = ConT $ mkName "String"
                userOtherPropType = (bang, AppT (ConT $ mkName "Maybe") userOtherPropRequiredType)
                userType =
                    Type
                        (mkName "User")
                        ( Map.fromList
                            [ (idKey, userIdType)
                            , (nameKey, userNameType)
                            , (otherPropKey, userOtherPropType)
                            ]
                        )
                        []
            describe "omit" $ do
                it "remove fields" $ do
                    (userWithId, logs) <- testTM (omit [nameKey, otherPropKey] userType)
                    logs `shouldBe` []
                    length (fields userWithId) `shouldBe` length (fields userType) - 2
                    Map.lookup idKey (fields userWithId) `shouldNotBe` Nothing
                    Map.lookup "otherProp" (fields userWithId) `shouldBe` Nothing
                    Map.lookup nameKey (fields userWithId) `shouldBe` Nothing
                describe "issue warning" $ do
                    it "field does not exist" $ do
                        (user2, logs) <- testTM (omit [nameKey, "idonotexist"] userType)
                        logs `shouldBe` ["No field 'idonotexist' in type."]
                        length (fields user2) `shouldBe` length (fields userType) - 1

            describe "required" $ do
                it "mark field as required" $ do
                    (user2, logs) <- testTM (require [otherPropKey] userType)
                    logs `shouldBe` []
                    snd <$> Map.lookup otherPropKey (fields user2)
                        `shouldBe` Just userOtherPropRequiredType
                describe "issue warning" $ do
                    it "field does not exist" $ do
                        (user2, logs) <- testTM (require ["idonotexist"] userType)
                        logs `shouldBe` ["field 'idonotexist' does not exist in type."]
                        fields user2 `shouldBe` fields userType
                    it "field is not optional" $ do
                        (user2, logs) <- testTM (require [idKey] userType)
                        logs `shouldBe` ["field 'id' is not optional."]
                        fields user2 `shouldBe` fields userType

            describe "pick" $ do
                it "pick fields" $ do
                    (userWithId, logs) <- testTM (pick [idKey] userType)
                    logs `shouldBe` []
                    length (fields userWithId) `shouldBe` 1
                    Map.lookup idKey (fields userWithId) `shouldBe` Just userIdType
                describe "issue warning" $ do
                    it "field does not exist" $ do
                        (emptyType, logs) <- testTM (pick ["a", idKey] userType)
                        logs `shouldBe` ["field 'a' does not exists in type."]
                        length (fields emptyType) `shouldBe` 0
                    it "result type is empty" $ do
                        (emptyType, logs) <- testTM (pick [] userType)
                        logs `shouldBe` ["result type is empty."]
                        length (fields emptyType) `shouldBe` 0

                    it "field does not exist and result type is empty" $ do
                        (emptyType, logs) <- testTM (pick ["a"] userType)
                        logs
                            `shouldBe` [ "field 'a' does not exist in type."
                                       , "result type is empty."
                                       ]
                        length (fields emptyType) `shouldBe` 0

            describe "intersection" $ do
                it "should have only common fields" $ do
                    (userWithId, _) <- testTM (pick [idKey] userType)
                    (intersectionRes, logs) <- testTM (intersection userType userWithId)
                    logs `shouldBe` []
                    Map.toList (fields intersectionRes) `shouldBe` [(idKey, userIdType)]
                describe "issue warning" $ do
                    it "result type is empty" $ do
                        (emptyType, logs) <- testTM (pick ["a"] userType)
                        logs `shouldBe` ["field 'a' does not exists in type."]
                        length (fields emptyType) `shouldBe` 0

            describe "union" $ do
                it "should have all fields" $ do
                    (userWithId, _) <- testTM (pick [idKey] userType)
                    (unionRes, logs) <- testTM (union userType userWithId)
                    logs `shouldBe` []
                    fields unionRes `shouldBe` fields userType
                it "should prefer the first type" $ do
                    (user2, _) <- testTM (require [otherPropKey] userType)
                    (unionRes, logs) <- testTM (union user2 userType)
                    logs `shouldBe` []
                    length (fields unionRes) `shouldBe` 3
                    snd <$> Map.lookup otherPropKey (fields unionRes)
                        `shouldBe` Just userOtherPropRequiredType
                it "should prefer the second type" $ do
                    (user2, _) <- testTM (require [otherPropKey] userType)
                    (unionRes, logs) <- testTM (union' user2 userType)
                    logs `shouldBe` []
                    length (fields unionRes) `shouldBe` 3
                    Map.lookup otherPropKey (fields unionRes)
                        `shouldBe` Just userOtherPropType

            describe "partial" $ do
                it "should wrap all fields with Maybe, except optional ones" $ do
                    (partialUser, logs) <- testTM (partial userType)
                    logs `shouldBe` []
                    Map.lookup otherPropKey (fields partialUser)
                        `shouldBe` Map.lookup otherPropKey (fields userType)
                    case snd <$> Map.lookup idKey (fields partialUser) of
                        Just (AppT (ConT wrapper) wrapped) -> do
                            nameBase wrapper `shouldBe` "Maybe"
                            wrapped `shouldBe` snd userIdType
                        x -> expectationFailure ("expected a type wrapped in maybe, got " ++ show x)
                it "should wrap all fields with Maybe, including optional ones" $ do
                    (partialUser, logs) <- testTM (partial' userType)
                    logs `shouldBe` []
                    forM_ (Map.toList $ fields userType) $ \(fName, (_, fType)) ->
                        case snd <$> Map.lookup fName (fields partialUser) of
                            Just (AppT (ConT wrapper) wrapped) -> do
                                nameBase wrapper `shouldBe` "Maybe"
                                wrapped `shouldBe` fType
                            x -> expectationFailure ("expected a type wrapped in maybe, got " ++ show x)
  where
    testTM = runQ . execTM

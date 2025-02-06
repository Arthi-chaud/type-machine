module TestTypeMachine.Functions (specs) where

import Control.Exception.Base
import Control.Monad
import Data.Either (isLeft)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Language.Haskell.TH hiding (bang)
import Test.Hspec
import TypeMachine
import TypeMachine.Log
import TypeMachine.TM (execTM)
import TypeMachine.Type (Type (..))

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
                typeWithVar =
                    Type
                        (mkName "TypeWithVar")
                        ( Map.fromList
                            [ ("a", (bang, VarT $ mkName "a"))
                            ,
                                ( "b"
                                ,
                                    ( bang
                                    , ConT (mkName "Maybe")
                                        `AppT` ( ConT (mkName "Maybe")
                                                    `AppT` VarT (mkName "b")
                                               )
                                    )
                                )
                            ]
                        )
                        [("a", Nothing), ("b", Nothing)]
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
                        (user2, logs) <- testTM (omit [nameKey, idKey, otherPropKey] userType)
                        logs `shouldBe` [emptyResultType]
                        length (fields user2) `shouldBe` 0
                    it "field does not exist" $ do
                        (user2, logs) <- testTM (omit [nameKey, "idonotexist"] userType)
                        logs `shouldBe` [fieldNotInType "idonotexist"]
                        length (fields user2) `shouldBe` length (fields userType) - 1
                describe "fail" $ do
                    it "has type parameters" $ do
                        shouldFail (omit [nameKey] typeWithVar)

            describe "required" $ do
                it "mark field as required" $ do
                    (user2, logs) <- testTM (require [otherPropKey] userType)
                    logs `shouldBe` []
                    snd <$> Map.lookup otherPropKey (fields user2)
                        `shouldBe` Just userOtherPropRequiredType
                describe "issue warning" $ do
                    it "field does not exist" $ do
                        (user2, logs) <- testTM (require ["idonotexist"] userType)
                        logs `shouldBe` [fieldNotInType "idonotexist"]
                        fields user2 `shouldBe` fields userType
                    it "field is not optional" $ do
                        (user2, logs) <- testTM (require [idKey] userType)
                        logs `shouldBe` [fieldNotOptional "id"]
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
                        logs `shouldBe` [fieldNotInType "a"]
                        length (fields emptyType) `shouldBe` 1
                    it "result type is empty" $ do
                        (emptyType, logs) <- testTM (pick [] userType)
                        logs `shouldBe` [emptyResultType]
                        length (fields emptyType) `shouldBe` 0

                    it "field does not exist and result type is empty" $ do
                        (emptyType, logs) <- testTM (pick ["a"] userType)
                        logs
                            `shouldBe` [ fieldNotInType "a"
                                       , emptyResultType
                                       ]
                        length (fields emptyType) `shouldBe` 0
                describe "fail" $ do
                    it "has type parameters" $ do
                        shouldFail (pick [nameKey] typeWithVar)

            describe "intersection" $ do
                it "should have only common fields" $ do
                    (userWithId, _) <- testTM (pick [idKey] userType)
                    (intersectionRes, logs) <- testTM (intersection userType userWithId)
                    logs `shouldBe` []
                    Map.toList (fields intersectionRes) `shouldBe` [(idKey, userIdType)]
                describe "issue warning" $ do
                    it "result type is empty" $ do
                        (emptyType, _) <- testTM (pick [] userType)
                        (res, logs) <- testTM (intersection emptyType userType)
                        logs `shouldBe` [emptyResultType]
                        length (fields res) `shouldBe` 0
                describe "fail" $ do
                    it "has type parameters" $ do
                        shouldFail (intersection userType typeWithVar)

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
                describe "fail" $ do
                    it "has type parameters" $ do
                        shouldFail (union userType typeWithVar)

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

            describe "record" $ do
                it "should have all fields" $ do
                    let fType = ConT (mkName "Maybe") `AppT` ConT (mkName "Int")
                    (res, logs) <- testTM (record ["a", "b"] (return fType))
                    logs `shouldBe` []
                    length (fields res) `shouldBe` 2
                    snd <$> Map.lookup "a" (fields res) `shouldBe` Just fType
                describe "issue warning" $ do
                    it "empty record" $ do
                        (emptyType, logs) <- testTM (record [] (conT $ mkName "Int"))
                        logs `shouldBe` [emptyResultType]
                        length (fields emptyType) `shouldBe` 0
                    it "duplicate key" $ do
                        (res, logs) <- testTM (record ["a", "a"] (conT $ mkName "Int"))
                        logs `shouldBe` [duplicateKey]
                        length (fields res) `shouldBe` 1

            describe "apply" $ do
                it "should replace type variables" $ do
                    (res, logs) <- testTM (applyMany [[t|Int|], [t|String|]] typeWithVar)
                    typeParams res `shouldBe` []
                    logs `shouldBe` []
                    -- Check first type variable
                    let aField = snd $ fromJust $ Map.lookup "a" $ fields res
                    case aField of
                        ConT n -> nameBase n `shouldBe` "Int"
                        t -> expectationFailure $ "expected a type constructor, got: " ++ show t
                    --  Check second type variable
                    logs `shouldBe` []
                    let bField = snd $ fromJust $ Map.lookup "b" $ fields res
                    case bField of
                        AppT (ConT _) (AppT (ConT _) (ConT n)) -> nameBase n `shouldBe` "String"
                        t ->
                            expectationFailure $
                                "expected two nexted type constructors, got: " ++ show t
                describe "issue warning" $ do
                    it "no type variable" $ do
                        (_, logs) <- testTM (apply [t|Int|] userType)
                        logs `shouldBe` [noTypeParameter]
  where
    testTM = runQ . execTM
    shouldFail tm = do
        mres <-
            try
                (testTM tm) ::
                IO (Either IOException (TypeMachine.Type, [String]))
        mres `shouldSatisfy` isLeft

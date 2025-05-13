{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Gen where

import Control.Monad (forM)
import Control.Monad.Writer.Strict
import Data.Extensible
import Data.Map.Strict (fromList)
import Language.Haskell.TH hiding (Type)
import qualified Language.Haskell.TH as TH
import SuperRecord
import TypeMachine
import TypeMachine.Type (Type (Type))

genWithFieldCount :: Int -> Q [Dec]
genWithFieldCount n = do
    tm <- genTMRecord n
    superr <- genSuperrecord n
    extensible <- genExtensible n
    return $ tm ++ superr ++ extensible

withNFields :: Int -> String -> (Int -> Q TH.Type) -> TM Type
withNFields n fieldPrefix getType = do
    fields <- forM [0 .. n - 1] $ \i -> do
        let fieldName = fieldPrefix ++ show i
        fieldType <- lift $ getType i
        return (fieldName, (Bang NoSourceUnpackedness SourceStrict, fieldType))
    return (Type (TH.mkName "_") (fromList fields) [])

genTMRecord :: Int -> Q [Dec]
genTMRecord n = do
    let tyName = "RecordTM" ++ show n
    recDef <- type_ tyName $ withNFields n ("field" ++ show n ++ "_") (const [t|Int|])
    builder <- genTMRecordBuilder tyName n
    sumF <- genTMRecordSum tyName n
    return $ recDef ++ builder ++ sumF

genTMRecordBuilder :: String -> Int -> Q [Dec]
genTMRecordBuilder tyName fieldCount =
    return
        [ PragmaD $ InlineP (mkName fName) NoInline FunLike AllPhases
        , FunD
            (mkName fName)
            [Clause [] (NormalB constructorApp) []]
        ]
  where
    constructorApp = foldl (\f arg -> AppE f (LitE $ IntegerL $ fromIntegral arg)) (ConE $ mkName tyName) [0 .. fieldCount - 1]
    fName = "mk" ++ tyName

genTMRecordSum :: String -> Int -> Q [Dec]
genTMRecordSum tyName fieldCount = do
    body_ <- foldl (\rest arg -> [|$rest + $(varE arg)|]) [|0|] fieldVarNames
    sig <- sigD (mkName fName) [t|$(conT $ mkName tyName) -> Int|]
    return
        [ sig
        , PragmaD $ InlineP (mkName fName) NoInline FunLike AllPhases
        , FunD (mkName fName) [Clause [pattern_] (NormalB body_) []]
        ]
  where
    pattern_ = ConP (mkName tyName) [] (VarP <$> fieldVarNames)
    fieldVarNames = mkName . ("f" ++) . show <$> [0 .. fieldCount - 1]
    fName = "sum" ++ tyName

----------

genSuperrecord :: Int -> Q [Dec]
genSuperrecord n = do
    let tyName = "SuperRRecord" ++ show n
    recDef <- genSuperrecordType tyName n
    builder <- genSuperrecordBuilder tyName n
    sum_ <- genSuperrecordSum tyName n
    return $ recDef : builder ++ sum_

genSuperrecordType :: String -> Int -> Q Dec
genSuperrecordType tyName fCount = do
    fields <- forM [0 .. fCount - 1] $ \i -> do
        let fName = getSuperrecordFieldName fCount i
        [t|$(return $ LitT $ StrTyLit fName) := Int|]
    ty <- foldr (\arg rest -> [t|$(return arg) ': $rest|]) (return PromotedNilT) fields
    return $ TySynD (mkName tyName) [] ty

getSuperrecordFieldName :: Int -> Int -> String
getSuperrecordFieldName fCount idx = "fSR" ++ show fCount ++ "_" ++ show idx

genSuperrecordBuilder :: String -> Int -> Q [Dec]
genSuperrecordBuilder tyName fCount = do
    body_ <-
        foldr
            ( \arg rest ->
                let label = LabelE $ getSuperrecordFieldName fCount arg
                 in [|$(return label) := ($(litE $ IntegerL $ fromIntegral arg) :: Int) & $rest|]
            )
            [|rnil|]
            [0 .. fCount - 1]
    sig <- sigD fName [t|SuperRecord.Record $(conT $ mkName tyName)|]

    return
        [ sig
        , PragmaD $ InlineP fName NoInline FunLike AllPhases
        , FunD fName [Clause [] (NormalB body_) []]
        ]
  where
    fName = mkName $ "mk" ++ tyName

genSuperrecordSum :: String -> Int -> Q [Dec]
genSuperrecordSum tyName fCount = do
    body_ <-
        foldl
            ( \rest arg ->
                let fieldLabel = labelE $ getSuperrecordFieldName fCount arg
                    fieldGetter = [|get $fieldLabel $(varE rVar)|]
                 in [|$rest + $fieldGetter|]
            )
            [|0|]
            [0 .. fCount - 1]
    sig <- sigD fName [t|SuperRecord.Record $(conT $ mkName tyName) -> Int|]
    return
        [ sig
        , PragmaD $ InlineP fName NoInline FunLike AllPhases
        , FunD fName [Clause [VarP rVar] (NormalB body_) []]
        ]
  where
    rVar = mkName "r"
    fName = mkName $ "sum" ++ tyName

--------------

genExtensible :: Int -> Q [Dec]
genExtensible n = do
    let tyName = "ExtensibleRecord" ++ show n
    recTy <- genExtensibleType tyName n
    builder <- genExtensibleBuilder tyName n
    sum_ <- genExtensibleSum tyName n
    return $ recTy ++ builder ++ sum_

getExtensibleFieldName :: Int -> Int -> String
getExtensibleFieldName fCount idx = "fE" ++ show fCount ++ "_" ++ show idx

genExtensibleType :: String -> Int -> Q [Dec]
genExtensibleType tyName fCount = do
    f <- mkField (unwords fieldNames)
    fields <- forM fieldNames $ \fName -> do [t|$(return $ LitT $ StrTyLit fName) Data.Extensible.:> Int|]
    fieldsTy <- foldr (\arg rest -> [t|$(return arg) ': $rest|]) (return PromotedNilT) fields
    let fieldsTyName = mkName $ tyName ++ "Fields"
    ty <- [t|Data.Extensible.Record $(conT fieldsTyName)|]
    return $
        f
            ++ [ TySynD fieldsTyName [] fieldsTy
               , TySynD (mkName tyName) [] ty
               ]
  where
    fieldNames = getExtensibleFieldName fCount <$> [0 .. fCount - 1]

genExtensibleBuilder :: String -> Int -> Q [Dec]
genExtensibleBuilder tyName fCount = do
    body_ <-
        foldr
            ( \arg rest ->
                let label = LabelE $ getExtensibleFieldName fCount arg
                 in [|$(return label) @= (($(litE $ IntegerL $ fromIntegral arg)) :: Int) <: $rest|]
            )
            [|emptyRecord|]
            [0 .. fCount - 1]

    sig <- sigD fName [t|$(conT $ mkName tyName)|]
    return
        [ sig
        , PragmaD $ InlineP fName NoInline FunLike AllPhases
        , FunD fName [Clause [] (NormalB body_) []]
        ]
  where
    fName = mkName $ "mk" ++ tyName

genExtensibleSum :: String -> Int -> Q [Dec]
genExtensibleSum tyName fCount = do
    body_ <-
        foldl
            ( \rest arg ->
                let fieldLabel = labelE $ getExtensibleFieldName fCount arg
                    fieldGetter = [|$(varE rVar) ^. $fieldLabel|]
                 in [|$rest + $fieldGetter|]
            )
            [|0|]
            [0 .. fCount - 1]
    sig <- sigD fName [t|$(conT $ mkName tyName) -> Int|]
    return
        [ sig
        , PragmaD $ InlineP fName NoInline FunLike AllPhases
        , FunD fName [Clause [VarP rVar] (NormalB body_) []]
        ]
  where
    rVar = mkName "r"
    fName = mkName $ "sum" ++ tyName

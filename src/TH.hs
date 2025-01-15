{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module TH where

import Control.Monad (forM)
import Data.Char (toUpper)
import Data.Foldable (find)
import Data.Functor ((<&>))
import GHC.Records (HasField (getField))
import Language.Haskell.TH
import Text.Printf (printf)

data TMType = TMType
    { name :: Name
    , fields :: [VarBangType]
    , typeParams :: [(Name, Maybe Kind)]
    }

tmTypeToTHType :: TMType -> Dec
tmTypeToTHType (TMType n f tp) =
    DataD
        []
        n
        (tpToTyVarBndrs <$> tp)
        Nothing
        [RecC (mkName $ nameBase n) f]
        []
  where
    tpToTyVarBndrs (varName, Nothing) = PlainTV varName BndrInvis
    tpToTyVarBndrs (varName, Just k) = KindedTV varName BndrInvis k

thTypeToTMType :: (MonadFail m) => Dec -> m TMType
thTypeToTMType (DataD _ tyName tybndrs _ cons _) = do
    vbt <- getRecordConstructorVars cons
    let tparams =
            tybndrs <&> \case
                PlainTV n _ -> (n, Nothing)
                KindedTV n _ k -> (n, Just k)
    return $ TMType tyName vbt tparams
thTypeToTMType _ = fail "Unsupported data type"

getRecordConstructorVars :: (MonadFail m) => [Con] -> m [VarBangType]
getRecordConstructorVars [RecC _ vbt] = return vbt
getRecordConstructorVars _ = fail "Type-Machine Error: Expected type to have exactly one Record constructor"

type_ :: String -> Name -> Q [Dec]
type_ newTyName source = do
    (TyConI ty) <- reify source
    tmType <- thTypeToTMType ty
    let typeWithoutName =
            tmType
                { name = mkName newTyName
                , fields = filter (\(n, _, _) -> nameBase n /= "name") (fields tmType)
                }
    return [tmTypeToTHType typeWithoutName]

defineIs :: Name -> Q [Dec]
defineIs tyName = do
    (TyConI (DataD _ _ _ _ cons _)) <- reify tyName
    classTypeVar <- newName "a"
    vbts <- getRecordConstructorVars cons
    let classFuncs = vbtToFunDec classTypeVar <$> vbts
    return [ClassD [] (isClassName tyName) [PlainTV classTypeVar BndrReq] [] classFuncs]
  where
    vbtToFunDec :: Name -> VarBangType -> Dec
    vbtToFunDec classtypeVar (n, _, t) =
        let
            memberName = mkName $ (("get" ++) . capitalize) $ nameBase n
         in
            SigD memberName (AppT (AppT ArrowT (VarT classtypeVar)) t)

capitalize :: String -> String
capitalize (c : cs) = toUpper c : cs
capitalize cs = cs

-- User -> IsUser
isClassName :: Name -> Name
isClassName tyName = mkName $ "Is" ++ capitalize (nameBase tyName)

deriveIs :: Name -> Name -> Q [Dec]
deriveIs sourceTypeName destTypeName = do
    (TyConI destType) <- reify destTypeName
    (TyConI sourceType) <- reify sourceTypeName
    TMType _ destVbts _ <- thTypeToTMType destType
    TMType _ sourceVbts _ <- thTypeToTMType sourceType
    let className = mkName ("Is" ++ (nameBase sourceTypeName))
    classFuncs <- forM sourceVbts $ \vbt@(n, _, _) -> case getVbrByName n destVbts of
        Nothing ->
            fail
                ( printf
                    "Type-Machine Error: Cannot define instance of %s for %s. Field %s is missing in %s "
                    (nameBase className)
                    (nameBase destTypeName)
                    (nameBase n)
                    (nameBase destTypeName)
                )
        Just _ -> vbtToFunDec vbt
    return [InstanceD Nothing [] (AppT (ConT className) (ConT destTypeName)) classFuncs]
  where
    getVbrByName :: Name -> [VarBangType] -> Maybe VarBangType
    getVbrByName n = find (\(n1, _, _) -> nameBase n == nameBase n1)
    vbtToFunDec :: VarBangType -> Q Dec
    vbtToFunDec (n, _, _) =
        let
            memberName = mkName $ (("get" ++) . capitalize) $ nameBase n
            expr = [|getField @($(litT $ strTyLit $ nameBase n))|]
         in
            funD memberName [clause [] (normalB expr) []]

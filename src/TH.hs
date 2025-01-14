{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

module TH where

import Control.Monad (forM)
import Data.Char (toUpper)
import Data.Foldable (find)
import GHC.Records (HasField (getField))
import Language.Haskell.TH
import Text.Printf (printf)

type_ :: String -> Name -> Q [Dec]
type_ newTyName source = do
    (TyConI (DataD ctx _ tyVarBinds kind cons l)) <- reify source
    let consWithoutName = (\(RecC _ vbt) -> RecC (mkName newTyName) $ filter (\(n, _, _) -> nameBase n /= "name") vbt) <$> cons
    return [DataD ctx (mkName newTyName) tyVarBinds kind consWithoutName l]

defineIs :: Name -> Q [Dec]
defineIs tyName = do
    (TyConI (DataD _ _ _ _ cons _)) <- reify tyName
    classTypeVar <- newName "a"
    vbts <- getRecordConstructorVars cons
    let classFuncs = vbtToFunDec classTypeVar <$> vbts
    return [ClassD [] (isClassName tyName) [PlainTV classTypeVar BndrReq] [] classFuncs]
  where
    vbtToFunDec :: Name -> VarBangType -> Dec
    vbtToFunDec classtypeVar (name, _, t) =
        let
            memberName = mkName $ (("get" ++) . capitalize) $ nameBase name
         in
            SigD memberName (AppT (AppT ArrowT (VarT classtypeVar)) t)

capitalize :: String -> String
capitalize (c : cs) = toUpper c : cs
capitalize cs = cs

-- User -> IsUser
isClassName :: Name -> Name
isClassName tyName = mkName $ "Is" ++ capitalize (nameBase tyName)

deriveIs :: Name -> Name -> Q [Dec]
deriveIs sourceType destType = do
    (TyConI (DataD _ _ _ _ destCons _)) <- reify destType
    (TyConI (DataD _ _ _ _ sourceCons _)) <- reify sourceType
    destVbts <- getRecordConstructorVars destCons
    sourceVbts <- getRecordConstructorVars sourceCons
    let className = isClassName sourceType
    classFuncs <- forM sourceVbts $ \vbt@(n, _, _) -> case getVbrByName n destVbts of
        Nothing ->
            fail
                ( printf
                    "Type-Machine Error: Cannot define instance of %s for %s. Field %s is missing in %s "
                    (nameBase className)
                    (nameBase destType)
                    (nameBase n)
                    (nameBase destType)
                )
        Just _ -> vbtToFunDec vbt
    return [InstanceD Nothing [] (AppT (ConT className) (ConT destType)) classFuncs]
  where
    getVbrByName :: Name -> [VarBangType] -> Maybe VarBangType
    getVbrByName name = find (\(n, _, _) -> nameBase n == nameBase name)
    vbtToFunDec :: VarBangType -> Q Dec
    vbtToFunDec (name, _, _) =
        let
            memberName = mkName $ (("get" ++) . capitalize) $ nameBase name
            expr = [|getField @($(litT $ strTyLit $ nameBase name))|]
         in
            funD memberName [clause [] (normalB expr) []]

getRecordConstructorVars :: (MonadFail m) => [Con] -> m [VarBangType]
getRecordConstructorVars [RecC _ vbt] = return vbt
getRecordConstructorVars _ = fail "Type-Machine Error: Expected type to have exactly one Record constructor"

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoFieldSelectors #-}

module TH where

import Data.Char (toUpper)
import GHC.Records (HasField (getField))
import Language.Haskell.TH

type_ :: String -> Name -> Q [Dec]
type_ newTyName source = do
    (TyConI (DataD ctx _ tyVarBinds kind cons l)) <- reify source
    let consWithoutName = (\(RecC _ vbt) -> RecC (mkName newTyName) $ filter (\(n, _, _) -> nameBase n /= "name") vbt) <$> cons
    is <- deriveIs source (mkName newTyName)
    return $ is ++ [DataD ctx (mkName newTyName) tyVarBinds kind consWithoutName l]

defineIs :: Name -> Q [Dec]
defineIs tyName = do
    (TyConI (DataD _ _ _ _ cons _)) <- reify tyName
    classTypeVar <- newName "a"
    RecC _ vbt <- case cons of
        [a] -> return a
        _ -> fail "Expected type to have exactly one constructor"
    let classFuncs = vbtToFunDec classTypeVar <$> vbt
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
    (TyConI (DataD _ _ _ _ cons _)) <- reify sourceType
    RecC _ vbt <- case cons of
        [a] -> return a
        _ -> fail "Expected type to have exactly one constructor"
    let className = isClassName sourceType
    classFuncs <- vbtToFunDec `mapM` vbt
    return [InstanceD Nothing [] (AppT (ConT className) (ConT destType)) classFuncs]
  where
    vbtToFunDec :: VarBangType -> Q Dec
    vbtToFunDec (name, _, _) =
        let
            memberName = mkName $ (("get" ++) . capitalize) $ nameBase name
            expr = [|getField @($(litT $ strTyLit $ nameBase name))|]
         in
            funD memberName [clause [] (normalB expr) []]

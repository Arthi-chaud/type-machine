{-# LANGUAGE TypeApplications #-}

module TypeMachine.TH.Is (isClassName, deriveIs, defineIs) where

import Control.Monad (forM)
import Data.List (find)
import Language.Haskell.TH hiding (Type, reifyType)
import Text.Printf
import TypeMachine.TH.Internal.Type
import TypeMachine.TH.Internal.Utils

-- | Get the name of the 'Is' class generated for the given type
--
-- @
-- > isClassName ''User
--   IsUser
-- @
isClassName :: Name -> Name
isClassName tyName = mkName $ "Is" ++ capitalize (nameBase tyName)

-- | Returns the declaration of the instance of 'Is' for a given type
--
-- @
--  > deriveIs ''Animal ''Dog
--
--    instance IsAnimal Dog where
--       ...
-- @
deriveIs :: Name -> Name -> Q [Dec]
deriveIs sourceTypeName destTypeName = do
    destVbts <- fields <$> reifyType destTypeName
    sourceVbts <- fields <$> reifyType sourceTypeName
    let className = mkName ("Is" ++ nameBase sourceTypeName)
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

-- | Define the 'Is' class for the given type
--
-- @
--  > data User = User { id :: Int, name :: String }
--  > defineIs ''User
--
--  class IsUser a where
--       getId :: a -> Int
--       getName :: a -> String
-- @
defineIs :: Name -> Q [Dec]
defineIs tyName = do
    ty <- reifyType tyName
    classTypeVar <- newName "a"
    let vbts = fields ty
    let classFuncs = vbtToFunDec classTypeVar <$> vbts
    return [ClassD [] (isClassName tyName) [PlainTV classTypeVar BndrReq] [] classFuncs]
  where
    vbtToFunDec :: Name -> VarBangType -> Dec
    vbtToFunDec classtypeVar (n, _, t) =
        let
            memberName = mkName $ (("get" ++) . capitalize) $ nameBase n
         in
            SigD memberName (AppT (AppT ArrowT (VarT classtypeVar)) t)

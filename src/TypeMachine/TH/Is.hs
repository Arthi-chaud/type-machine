{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module TypeMachine.TH.Is (isClassName, toFuncName, deriveIs, defineIs) where

import Control.Monad (MonadPlus (mzero), forM)
import qualified Data.Map.Strict as Map
import GHC.Records (getField)
import Language.Haskell.TH hiding (Type, reifyType)
import qualified Language.Haskell.TH as TH
import Text.Printf
import TypeMachine.TH.Internal.Utils
import TypeMachine.Type (fields, reifyType)

-- | Get the name of the 'Is' class generated for the given type
--
-- @
-- > isClassName ''User
--   IsUser
-- @
isClassName :: Name -> Name
isClassName = mkName . ("Is" ++) . capitalize . nameBase

-- | Get the name of the 'To' function generated for the given type
--
-- @
-- > toFuncName ''User
--   ToUser
-- @
toFuncName :: Name -> Name
toFuncName = mkName . ("to" ++) . capitalize . nameBase

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
    destFields <- fields <$> reifyType destTypeName
    sourceFields <- fields <$> reifyType sourceTypeName
    let className = mkName ("Is" ++ nameBase sourceTypeName)
    classFuncs <- forM (Map.toList sourceFields) $ \(n, (_, t)) ->
        case Map.lookup n destFields of
            Just _ -> fieldNameToFunDec n
            Nothing ->
                ifM
                    (fieldIsOptional t)
                    (fieldNameToMemptyFunDec n)
                    ( fail
                        ( printf
                            "Type-Machine Error: Cannot define instance of %s for %s. Field '%s' is missing in %s "
                            (nameBase className)
                            destTypeStr
                            n
                            destTypeStr
                        )
                    )
    return [InstanceD Nothing [] (AppT (ConT className) (ConT destTypeName)) classFuncs]
  where
    destTypeStr = nameBase destTypeName
    fieldNameToMemptyFunDec n =
        funD (mkName $ fieldNameToIsFuncName n) [clause [] (normalB [|const mzero|]) []]
    fieldNameToFunDec n =
        let
            memberName = mkName $ fieldNameToIsFuncName n
            expr = [|getField @($(litT $ strTyLit n))|]
         in
            funD memberName [clause [] (normalB expr) []]
    -- Returns true is field is instance of Monad plus
    -- TODO handle non-parametric monadplus-es
    fieldIsOptional :: TH.Type -> Q Bool
    fieldIsOptional (AppT t _) = isInstance ''MonadPlus [t]
    fieldIsOptional _ = return False
    ifM mbool t f = do bool <- mbool; if bool then t else f

-- | Define the 'Is' class for the given type and generate the 'To' function
--
-- @
--  > data User = User { id :: Int, name :: String }
--  > defineIs ''User
--
--  class IsUser a where
--       getId :: a -> Int
--       getName :: a -> String
--
--  toUser :: (IsUser a) => a -> User
--  toUser a = User (getId a) (getName a)
-- @
defineIs :: Name -> Q [Dec]
defineIs tyName = do
    ty <- reifyType tyName
    classTypeVar <- newName "a"
    let classFuncs = vbtToFunDec classTypeVar <$> Map.toList (fields ty)
    to <- defineTo tyName
    return $
        ClassD [] (isClassName tyName) [PlainTV classTypeVar BndrReq] [] classFuncs
            : to
  where
    -- vbtToFunDec a id Int == getId :: a -> Int
    vbtToFunDec :: Name -> (String, BangType) -> Dec
    vbtToFunDec classtypeVar (n, (_, t)) =
        let
            memberName = mkName $ fieldNameToIsFuncName n
         in
            SigD memberName (AppT (AppT ArrowT (VarT classtypeVar)) t)

-- | Generate the 'To' function
--
-- @
--  > data User = User { id :: Int, name :: String }
--  > defineIs ''User
--
--  toUser :: (IsUser a) => a -> User
--  toUser from = User (getId from) (getName from)
-- @
defineTo :: Name -> Q [Dec]
defineTo tyName = do
    ty <- reifyType tyName
    toFuncType <-
        sigD
            toName
            [t|forall a. ($(conT $ isClassName tyName) a) => a -> $(conT tyName)|]
    toFuncBody <-
        let
            from = mkName "from"
            app =
                foldl'
                    (\r n -> [|$r ($(varE $ mkName $ fieldNameToIsFuncName n) $(varE from))|])
                    (conE $ mkName $ nameBase tyName)
                    (Map.keys $ fields ty)
         in
            funD (toFuncName tyName) [clause [varP from] (normalB app) []]
    return [toFuncType, toFuncBody]
  where
    toName = toFuncName tyName

-- Internal
fieldNameToIsFuncName :: String -> String
fieldNameToIsFuncName = ("get" ++) . capitalize

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module TypeMachine.TH.Is (isClassName, deriveIs, defineIs) where

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
    destFields <- fields <$> reifyType destTypeName
    sourceFields <- fields <$> reifyType sourceTypeName
    let className = mkName ("Is" ++ nameBase sourceTypeName)
    classFuncs <- forM (Map.toList sourceFields) $ \(n, (_, t)) -> case Map.lookup n destFields of
        Just _ -> fieldNameToFunDec n
        Nothing -> do
            isOpt <- fieldIsOptional t
            if isOpt
                then fieldNameToMemptyFunDec n
                else
                    fail
                        ( printf
                            "Type-Machine Error: Cannot define instance of %s for %s. Field '%s' is missing in %s "
                            (nameBase className)
                            destTypeStr
                            n
                            destTypeStr
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
    let classFuncs = vbtToFunDec classTypeVar <$> Map.toList vbts
    return [ClassD [] (isClassName tyName) [PlainTV classTypeVar BndrReq] [] classFuncs]
  where
    -- vbtToFunDec a id Int == getId :: a -> Int
    vbtToFunDec :: Name -> (String, BangType) -> Dec
    vbtToFunDec classtypeVar (n, (_, t)) =
        let
            memberName = mkName $ fieldNameToIsFuncName n
         in
            SigD memberName (AppT (AppT ArrowT (VarT classtypeVar)) t)

-- Internal
fieldNameToIsFuncName :: String -> String
fieldNameToIsFuncName = ("get" ++) . capitalize

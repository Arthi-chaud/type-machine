module TypeMachine.TH.Is (isClassName, deriveIs, defineIs) where

import Control.Monad (MonadPlus (mzero), forM)
import Data.List (singleton)
import qualified Data.Map.Strict as Map
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

-- | Get the name of the 'to' function generated for the given type
--
-- @
-- > toFuncName ''User
--   toUser
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
    classFuncs <- fmap concat $ forM (zip [0 ..] $ Map.toList sourceFields) $ \(i, (n, (_, t))) ->
        case Map.lookup n destFields of
            Just _ -> do
                getter <- fieldToGetter n
                setter <- fieldToSetter (length destFields) i n
                return [getter, setter]
            Nothing ->
                ifM
                    (fieldIsOptional t)
                    (singleton <$> fieldNameToMemptyFunDec n)
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
        funD (mkName $ fieldNameToIsGetter n) [clause [] (normalB [|const mzero|]) []]
    fieldToGetter n = do
        let funName = mkName $ fieldNameToIsGetter n
            resName = mkName "res"
            expr = [|$(varE resName)|]
        -- Note: using destTypeName makes Q think that we use the type, not the constructor
        funD
            funName
            [clause [return $ RecP (mkName destTypeStr) [(mkName n, VarP resName)]] (normalB expr) []]

    fieldToSetter fieldCount fieldPos fieldName = do
        fieldsNames <-
            forM
                [0 .. (fieldCount - 1)]
                ( \i ->
                    if i == fieldPos
                        then return $ mkName "_"
                        else newName $ "f" ++ show i
                )
        let funName = mkName $ fieldNameToIsSetter fieldName
        let newValueName = mkName "new"
        let patt = ConP (mkName destTypeStr) [] (VarP <$> fieldsNames)
        let body =
                foldl
                    ( \res f ->
                        res `AppE` case nameBase f of
                            "_" -> VarE newValueName
                            _ -> VarE f
                    )
                    (ConE $ mkName destTypeStr)
                    fieldsNames
        funD funName [clause [varP newValueName, return $ patt] (normalB $ return body) []]
    -- TODO handle non-parametric monadplus-es
    -- Returns true is field is instance of Monad plus
    fieldIsOptional :: TH.Type -> Q Bool
    fieldIsOptional (AppT t _) = isInstance ''MonadPlus [t]
    fieldIsOptional _ = return False
    ifM mbool t f = do bool <- mbool; if bool then t else f

----- Definition

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
--       setId :: Int -> a -> a
--       setName :: String -> a -> a
--
--       toUser :: (IsUser a) => a -> User
--       toUser a = User (getId a) (getName a)
--
--  instance IsUser User where
--       ...
--
-- @
defineIs :: Name -> Q [Dec]
defineIs tyName = do
    ty <- reifyType tyName
    classTypeVar <- newName "a"
    getters <- mapM (vbtToGetter classTypeVar) (Map.toList $ fields ty)
    setters <- mapM (vbtToSetter classTypeVar) (Map.toList $ fields ty)
    to <- defineTo tyName classTypeVar
    isItself <- deriveIs tyName tyName
    return $
        ClassD
            []
            (isClassName tyName)
            [PlainTV classTypeVar BndrReq]
            []
            (getters ++ setters ++ to)
            : isItself
  where
    -- vbtToGetter a id Int == getId :: a -> Int
    vbtToGetter :: Name -> (String, BangType) -> Q Dec
    vbtToGetter classtypeVar (n, (_, t)) =
        let
            memberName = mkName $ fieldNameToIsGetter n
         in
            sigD memberName [t|$(varT classtypeVar) -> $(return t)|]

    -- vbtToSetter a id Int == setId ::  Int -> a -> a
    vbtToSetter :: Name -> (String, BangType) -> Q Dec
    vbtToSetter classtypeVar (n, (_, t)) =
        let
            memberName = mkName $ fieldNameToIsSetter n
         in
            sigD memberName [t|$(return t) -> $(varT classtypeVar) -> $(varT classtypeVar)|]

-- | Generate the 'To' function
--
-- @
--  > data User = User { id :: Int, name :: String }
--
--  toUser :: (IsUser a) => a -> User
--  toUser from = User (getId from) (getName from)
-- @
defineTo :: Name -> Name -> Q [Dec]
defineTo tyName tyVarName = do
    ty <- reifyType tyName
    toFuncType <-
        sigD
            toName
            [t|$(varT tyVarName) -> $(conT tyName)|]
    toFuncBody <-
        let
            from = mkName "from"
            app =
                foldl'
                    (\r n -> [|$r ($(varE $ mkName $ fieldNameToIsGetter n) $(varE from))|])
                    (conE $ mkName $ nameBase tyName)
                    (Map.keys $ fields ty)
         in
            funD (toFuncName tyName) [clause [varP from] (normalB app) []]
    return [toFuncType, toFuncBody]
  where
    toName = toFuncName tyName

-- Internal
fieldNameToIsGetter :: String -> String
fieldNameToIsGetter = ("get" ++) . capitalize

fieldNameToIsSetter :: String -> String
fieldNameToIsSetter = ("set" ++) . capitalize

module TypeMachine.Type (
    Type (..),
    getField,
    hasField,
    typeToDec,
    decToType,
    reifyType,
) where

import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Language.Haskell.TH.Syntax hiding (Type, reifyType)
import qualified Language.Haskell.TH.Syntax as TH (Type)
import TypeMachine.TH.Internal.Utils

-- | Data structure to easily manipulate Template Haskell's 'Language.Haskell.TH.Dec.DataD'
data Type = Type
    { name :: Name
    -- ^ Name of the data type
    , fields :: Map String BangType
    -- ^ Fields of the data type
    , typeParams :: [(String, Maybe Kind)]
    -- ^ Type parameter of the ADT
    }

getField :: String -> Type -> Maybe (Bang, TH.Type)
getField fieldName ty = Map.lookup fieldName (fields ty)

hasField :: String -> Type -> Bool
hasField n t = isJust $ getField n t

-- | Turns a 'Type' back to a Template Haskell 'Language.Haskell.TH.Dec'
typeToDec :: Type -> Dec
typeToDec (Type n fs tp) =
    DataD
        []
        n
        (tpToTyVarBndrs . first mkName <$> tp)
        Nothing
        [RecC (mkName $ nameBase n) $ fieldsToVbt fs]
        []
  where
    tpToTyVarBndrs (varName, Nothing) = PlainTV varName BndrInvis
    tpToTyVarBndrs (varName, Just k) = KindedTV varName BndrInvis k

-- | Transform a Template Haskell 'Dec' into a TypeMachine's type
--
-- For this to succeed, the input type must have exactly one record constructor
decToType :: (MonadFail m) => Dec -> m Type
decToType dec = do
    (tyName, tybndrs, vbt) <- case dec of
        (DataD _ tyName tybndrs _ cons _) -> (tyName,tybndrs,) <$> getRecordConstructorVars cons
        (NewtypeD _ tyName tybndrs _ con _) -> (tyName,tybndrs,) <$> getRecordConstructorVars [con]
        _ -> fail "Unsupported data type" -- TODO Clearer message
    let tparams =
            tybndrs <&> \case
                PlainTV n _ -> (n, Nothing)
                KindedTV n _ k -> (n, Just k)
    return $ Type tyName (vbtToFields vbt) (first nameBase <$> tparams)

-- | Wrapper around the TH's 'reify' function. Fails if the type is not a datatype declaration
reifyType :: Name -> Q Type
reifyType n = do
    res <- reify n
    case res of
        TyConI ty -> decToType ty
        _ -> fail "Invalid Name. Expected Datatype declaration"

{-# INLINE fieldsToVbt #-}
fieldsToVbt :: Map String BangType -> [VarBangType]
fieldsToVbt = Map.foldrWithKey (\key (b, t) list -> (mkName key, b, t) : list) []

{-# INLINE vbtToFields #-}
vbtToFields :: [VarBangType] -> Map String BangType
vbtToFields = Map.fromList . fmap (\(n, b, t) -> (nameBase n, (b, t)))

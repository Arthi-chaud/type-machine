module TypeMachine.TH.Internal.Type (
    Type (..),
    typeToDec,
    decToType,
    reifyType,
) where

import Data.Functor ((<&>))
import Language.Haskell.TH.Syntax hiding (Type, reifyType)
import TypeMachine.TH.Internal.Utils

-- | Data structure to easily manipulate Template Haskell's 'Language.Haskell.TH.Dec.DataD'
data Type = Type
    { name :: Name
    -- ^ Name of the data type
    , fields :: [VarBangType]
    -- ^ Fields of the data type
    , typeParams :: [(Name, Maybe Kind)]
    -- ^ Type parameter of the ADT
    -- TODO Store original type
    }

-- | Turns a 'Type' back to a Template Haskell 'Language.Haskell.TH.Dec'
typeToDec :: Type -> Dec
typeToDec (Type n f tp) =
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

-- | Transform a Template Haskell 'Dec' into a TypeMachine's type
--
-- For this to succeed, the input type must have exactly one record constructor
decToType :: (MonadFail m) => Dec -> m Type
decToType (DataD _ tyName tybndrs _ cons _) = do
    vbt <- getRecordConstructorVars cons
    let tparams =
            tybndrs <&> \case
                PlainTV n _ -> (n, Nothing)
                KindedTV n _ k -> (n, Just k)
    return $ Type tyName vbt tparams
decToType _ = fail "Unsupported data type" -- TODO Clearer message

-- | Wrapper around the TH's 'reify' function. Fails if the type is not a datatype declaration
reifyType :: Name -> Q Type
reifyType n = do
    res <- reify n
    case res of
        TyConI ty -> decToType ty
        _ -> fail "Invalid Name. Expected Datatype declaration"

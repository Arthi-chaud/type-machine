module TypeMachine.TH (removeField, requireField, type_, deriveIs, defineIs) where

import Language.Haskell.TH hiding (Type, reifyType)
import TypeMachine.TH.Internal.Type
import TypeMachine.TH.Is

removeField :: String -> Type -> Type
removeField nameToRemove ty = ty{fields = filteredFields}
  where
    -- TODO: Warning if field does not exist
    filteredFields = filter (\(n, _, _) -> nameBase n /= nameToRemove) $ fields ty

requireField :: String -> Type -> Type
requireField fieldNameToRequire ty = ty{fields = markAsRequired <$> fields ty}
  where
    -- TODO Handle any type that is monadplus
    markAsRequired (n, b, AppT (ConT p) t)
        | fieldNameToRequire == nameBase n && nameBase p == "Maybe" = (n, b, t)
    markAsRequired r = r

type_ :: String -> Name -> (Type -> Type) -> Q [Dec]
type_ newTyName source f = do
    tmType <- reifyType source
    let newType = (f tmType){name = mkName newTyName}
    return [typeToDec newType]

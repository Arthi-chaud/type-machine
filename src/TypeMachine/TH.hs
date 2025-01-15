module TypeMachine.TH (removeField, type_, deriveIs, defineIs) where

import Language.Haskell.TH hiding (Type, reifyType)
import TypeMachine.TH.Internal.Type
import TypeMachine.TH.Is

removeField :: String -> Type -> Type
removeField nameToRemove ty = ty{fields = filteredFields}
  where
    -- TODO: Warning if field does not exist
    filteredFields = filter (\(n, _, _) -> nameBase n /= nameToRemove) $ fields ty

type_ :: String -> Name -> (Type -> Type) -> Q [Dec]
type_ newTyName source f = do
    tmType <- reifyType source
    let newType = (f tmType){name = mkName newTyName}
    return [typeToDec newType]

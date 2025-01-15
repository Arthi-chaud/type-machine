module TypeMachine.TH (removeField, type_, deriveIs, defineIs) where

import Language.Haskell.TH hiding (Type, reifyType)
import TypeMachine.TH.Internal.Type
import TypeMachine.TH.Is

removeField :: Name -> Type -> Type
removeField nameToRemove ty = ty{fields = filteredFields}
  where
    -- TODO: Warning if field does not exist
    filteredFields = filter (\(n, _, _) -> nameBase n /= strNameToRemove) $ fields ty
    strNameToRemove = nameBase nameToRemove

type_ :: String -> Name -> Q [Dec]
type_ newTyName source = do
    tmType <- reifyType source
    let newType = (removeField (mkName "name") tmType){name = mkName newTyName}
    return [typeToDec newType]

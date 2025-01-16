module TypeMachine.TH (type_, deriveIs, defineIs) where

import Language.Haskell.TH hiding (Type, reifyType)
import TypeMachine.TH.Internal.Type
import TypeMachine.TH.Is
import TypeMachine.TypeFunction (TypeFunction, runTypeFunction)

-- | Entrypoint of TypeMachine. Create a new data type using a source type and a 'TypeFunction'
type_ :: String -> Name -> TypeFunction Type -> Q [Dec]
type_ newTyName source f = do
    tmType <- reifyType source
    fRes <- runTypeFunction tmType f
    let newType = fRes{name = mkName newTyName}
    return [typeToDec newType]

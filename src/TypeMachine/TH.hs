module TypeMachine.TH (type_, deriveIs, defineIs) where

import Language.Haskell.TH hiding (Type, reifyType)
import TypeMachine.TH.Is
import TypeMachine.Type
import TypeMachine.TypeFunction (T, runT)

-- | Entrypoint of TypeMachine. Create a new data type using a 'T' computation
type_ :: String -> T Type -> Q [Dec]
type_ newTyName t = do
    fRes <- runT t
    let newType = fRes{name = mkName newTyName}
    return [typeToDec newType]

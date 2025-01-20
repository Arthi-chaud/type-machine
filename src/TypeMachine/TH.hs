module TypeMachine.TH (type_, deriveIs, defineIs) where

import Language.Haskell.TH hiding (Type, reifyType)
import TypeMachine.TH.Is
import TypeMachine.TM (TM, runTM)
import TypeMachine.Type

-- | Entrypoint of TypeMachine. Create a new data type using a 'T' computation
type_ :: String -> TM Type -> Q [Dec]
type_ newTyName t = do
    fRes <- runTM t
    let newType = fRes{name = mkName newTyName}
    return [typeToDec newType]

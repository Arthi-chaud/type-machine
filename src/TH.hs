module TH where

import Control.Monad.IO.Class
import Language.Haskell.TH

type_ :: String -> String -> Q [Dec]
type_ newTyName source = do
    (TyConI (DataD ctx _ tyVarBinds kind cons l)) <- reify (mkName source)
    let consWithoutName = (\(RecC _ vbt) -> RecC (mkName newTyName) $ filter (\(n, _, _) -> nameBase n /= "name") vbt) <$> cons

    return [DataD ctx (mkName newTyName) tyVarBinds kind consWithoutName l]

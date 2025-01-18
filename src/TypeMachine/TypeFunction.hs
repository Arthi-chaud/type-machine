module TypeMachine.TypeFunction (
    TypeFunction,
    runTypeFunction,
    remove,
    require,
    pick,
) where

import Control.Monad (forM_, unless)
import Control.Monad.Writer.Lazy
import qualified Data.Map.Strict as Map
import Language.Haskell.TH hiding (Type)
import TypeMachine.Log (TypeMachineLog, formatLog)
import TypeMachine.Type

-- | A 'TypeFunction' is a function that takes a 'Type' as parameter and can:
--
-- - Emit warning messages (e.g. when omitting that does not exist)
-- - Take advantage of the 'Language.Haskell.TH.Q' monad's features
type TypeFunction a = Type -> WriterT [TypeMachineLog] Q a

-- | Execute a 'TypeFunction' and issue logs
runTypeFunction :: Type -> TypeFunction a -> Q a
runTypeFunction t f = do
    (res, logs) <- runWriterT (f t)
    forM_ logs $ reportWarning . formatLog
    return res

remove :: String -> TypeFunction Type
remove nameToRemove ty =
    if not (hasField nameToRemove ty)
        then tell ["No field '" ++ nameToRemove ++ "' in type."] >> return ty
        else return ty{fields = Map.delete nameToRemove (fields ty)}

require :: String -> TypeFunction Type
require fieldNameToRequire ty = return ty{fields = markAsRequired `Map.mapWithKey` fields ty}
  where
    -- TODO Handle any type that is monadplus
    markAsRequired n (b, AppT (ConT p) t)
        | fieldNameToRequire == n && nameBase p == "Maybe" = (b, t)
    markAsRequired _ r = r

pick :: [String] -> TypeFunction Type
pick namesToPick ty = do
    forM_ namesToPick $ \nameToPick ->
        unless (hasField nameToPick ty) $
            tell ["No field '" ++ nameToPick ++ "' in type."]
    return ty{fields = Map.filterWithKey (\k _ -> k `elem` namesToPick) (fields ty)}

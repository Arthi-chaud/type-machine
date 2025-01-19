module TypeMachine.TypeFunction (
    TypeFunction,
    runTypeFunction,
    remove,
    require,
    pick,
    intersection,
    keysOf,
) where

import Control.Monad (forM_, unless)
import Control.Monad.Writer.Lazy
import Data.Map.Merge.Strict
import qualified Data.Map.Strict as Map
import Language.Haskell.TH hiding (Type)
import TypeMachine.Internal.Utils (keepKeys)
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
    return ty{fields = keepKeys namesToPick (fields ty)}

-- | Merges to types together
--
--  Removes overloapping fields
-- @
--  data A = A { a :: Int, b :: Int }
--
--  data B = B { b :: String, c :: Void }
--
--  > merge B A
--
--  data _ = { a :: Int, c :: Void }
-- @
intersection :: Type -> TypeFunction Type
intersection a b = return $ a{fields = finalFields}
  where
    finalFields =
        merge
            preserveMissing
            preserveMissing
            (zipWithMaybeMatched (\_ _ _ -> Nothing))
            (fields a)
            (fields b)

-- | Get the names of the fields in in type
--
-- @
--  data A = A { a :: Int, b :: Int }
--
--  > keysOf A
--
--  ['a', 'b']
-- @
keysOf :: TypeFunction [String]
keysOf = return . Map.keys . fields

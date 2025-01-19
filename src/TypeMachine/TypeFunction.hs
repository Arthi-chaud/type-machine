module TypeMachine.TypeFunction (
    T,
    TypeFunction,
    runT,
    remove,
    require,
    pick,
    intersection,
    keysOf,
    toType,
) where

import Control.Monad (forM_, unless)
import Control.Monad.Writer.Lazy
import qualified Data.Map.Strict as Map
import Language.Haskell.TH hiding (Type, reifyType)
import TypeMachine.Internal.Utils (keepKeys)
import TypeMachine.Log (TypeMachineLog, formatLog)
import TypeMachine.Type

-- | The 'T' monad can:
--
-- - Emit warning messages (e.g. when omitting that does not exist)
-- - Take advantage of the 'Language.Haskell.TH.Q' monad's features
type T a = WriterT [TypeMachineLog] Q a

-- | A 'TypeFunction' is a 'T' computation that takes a 'Type' as parameter
type TypeFunction a = Type -> T a

-- | Execute a 'T' computation and issue logs
runT :: T a -> Q a
runT t = do
    (res, logs) <- runWriterT t
    forM_ logs $ reportWarning . formatLog
    return res

-- | Takes an ADT name, returns the `Type` for that ADT
--
-- A utilitary function to use 'reifyType' in the 'T' monad
toType :: Name -> T Type
toType = lift . reifyType

-- | Removes a single field by name
--
-- Issues a warning when a key is not in the input 'Type'
--
-- @
--  > data A = A { a :: Int, b :: Int }
--  > remove 'a' =<< toType ''A
--
--  data _ = { b :: Int }
-- @
remove :: String -> TypeFunction Type
remove nameToRemove ty =
    if not (hasField nameToRemove ty)
        then tell ["No field '" ++ nameToRemove ++ "' in type."] >> return ty
        else return ty{fields = Map.delete nameToRemove (fields ty)}

-- | Mark fields are required
--
-- @
--  > data A = A { a :: Maybe Int, b :: Int }
--  > require "a" =<< toType ''A
--
--  data _ = { a :: Int, b :: Int }
-- @
require :: String -> TypeFunction Type
require fieldNameToRequire ty = return ty{fields = markAsRequired `Map.mapWithKey` fields ty}
  where
    -- TODO Handle any type that is monadplus
    markAsRequired n (b, AppT (ConT p) t)
        | fieldNameToRequire == n && nameBase p == "Maybe" = (b, t)
    markAsRequired _ r = r

-- | Pick/Select fields by name
--
-- Issues a warning when a key is not in the input 'Type'
--
-- @
--  > data A = A { a :: Int, b :: Int }
--  > pick ["a", "c"] =<< toType ''A
--
--  data _ = { a :: Int }
-- @
pick :: [String] -> TypeFunction Type
pick namesToPick ty = do
    forM_ namesToPick $ \nameToPick ->
        unless (hasField nameToPick ty) $
            tell ["No field '" ++ nameToPick ++ "' in type."]
    return ty{fields = keepKeys namesToPick (fields ty)}

-- | Merges two types together. Removes overloapping fields
--
-- @
--  > data A = A { a :: Int, b :: Int }
--  > data B = B { b :: String, c :: Void }
--  > intersection B A
--
--  data _ = { a :: Int, c :: Void }
-- @
intersection :: Type -> TypeFunction Type
intersection a b = return $ a{fields = Map.intersection (fields a) (fields b)}

-- | Get the names of the fields in in type
--
-- @
--  > data A = A { a :: Int, b :: Int }
--  > keysOf A
--
--  ['a', 'b']
-- @
keysOf :: TypeFunction [String]
keysOf = return . Map.keys . fields

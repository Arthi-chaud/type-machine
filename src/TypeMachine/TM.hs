module TypeMachine.TM (
    TM,
    runTM,
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

-- | The 'TM' (*TypeMachine*) monad can:
--
-- - Emit warning messages (e.g. when omitting that does not exist)
-- - Take advantage of the 'Language.Haskell.TH.Q' monad's features
type TM a = WriterT [TypeMachineLog] Q a

-- | Execute a 'TM' computation and issue logs
runTM :: TM a -> Q a
runTM t = do
    (res, logs) <- runWriterT t
    forM_ logs $ reportWarning . formatLog
    return res

-- | Takes an ADT name, returns the `Type` for that ADT
--
-- A utilitary function to use 'reifyType' in the 'TM' monad
toType :: Name -> TM Type
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
remove :: String -> Type -> TM Type
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
require :: String -> Type -> TM Type
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
pick :: [String] -> Type -> TM Type
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
intersection :: Type -> Type -> TM Type
intersection a b = return $ a{fields = Map.intersection (fields a) (fields b)}

-- | Get the names of the fields in in type
--
-- @
--  > data A = A { a :: Int, b :: Int }
--  > keysOf A
--
--  ['a', 'b']
-- @
keysOf :: Type -> TM [String]
keysOf = return . Map.keys . fields

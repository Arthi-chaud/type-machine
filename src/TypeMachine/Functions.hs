module TypeMachine.Functions (
    -- * Fields
    pick,
    remove,
    omit,

    -- * Union and Intersection
    intersection,

    -- * Optional
    require,
    partial,
    partial',

    -- * Utils
    keysOf,
)
where

import Control.Monad (forM)
import Control.Monad.Writer.Strict
import qualified Data.Map.Strict as Map
import Language.Haskell.TH hiding (Type)
import TypeMachine.Internal.Utils
import TypeMachine.TM
import TypeMachine.Type

-- | Removes a single field by name
--
-- Issues a warning when a key is not in the input 'Type'
--
-- @
--  > data A = A { a :: Int, b :: Int }
--  > remove 'a' '<:>' 'toType' ''A
--
--  data _ = { b :: Int }
-- @
remove :: String -> Type -> TM Type
remove nameToRemove = omit [nameToRemove]

-- | Mark fields are required
--
-- @
--  > data A = A { a :: Maybe Int, b :: Int }
--  > require "a" '<:>' 'toType' ''A
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
--  > pick ["a", "c"] '<:>' 'toType' ''A
--
--  data _ = { a :: Int }
-- @
pick :: [String] -> Type -> TM Type
pick namesToPick ty = do
    tell $ (\n -> "No field '" ++ n ++ "' in type.") <$> unknownfields
    return ty{fields = keepKeys namesToPick (fields ty)}
  where
    unknownfields = filter (`hasField` ty) namesToPick

-- | Remove fields by name
--
-- Issues a warning when a key is not in the input 'Type'
--
-- @
--  > data A = A { a :: Int, b :: Int }
--  > pick ["b", "c"] '<:>' 'toType' ''A
--
--  data _ = { a :: Int }
-- @
omit :: [String] -> Type -> TM Type
omit namesToOmit ty = do
    tell $ (\n -> "No field '" ++ n ++ "' in type.") <$> unknownfields
    return ty{fields = removeKeys namesToOmit (fields ty)}
  where
    unknownfields = filter (`hasField` ty) namesToOmit

-- omit :: TODO

-- | Merges two types together. Removes overloapping fields
--
-- @
--  > data A = A { a :: Int, b :: Int }
--  > data B = B { b :: String, c :: Void }
--  > intersection '<:>' 'toType' ''B  <:> 'toType' ''A
--
--  data _ = { a :: Int, c :: Void }
-- @
intersection :: Type -> Type -> TM Type
intersection a b = return $ a{fields = Map.intersection (fields a) (fields b)}

-- | Get the names of the fields in in type
--
-- @
--  > data A = A { a :: Int, b :: Int }
--  > keysOf '<:>' 'toType' ''A
--
--  ["a", "b"]
-- @
keysOf :: Type -> TM [String]
keysOf = return . Map.keys . fields

-- | Marks all fields as 'Maybe'
--
--  Fields that already have a 'Maybe' type will not be changed.
--
-- @
--  > data A = A { a :: Int, b :: Maybe Int }
--  > partial '<:>' toType ''A
--
--  data _ = { a :: Maybe Int, b :: Maybe Int }
-- @
partial :: Type -> TM Type
partial = partial_ False

-- | Similar to 'partial'. Marks all fields as 'Maybe'
--
--  Fields that already have a 'Maybe' type will be wrapped in a 'Maybe'
--
-- @
--  > data A = A { a :: Int, b :: Maybe Int }
--  > partial' '<:>' toType ''A
--
--  data _ = { a :: Maybe Int, b :: Maybe (Maybe Int) }
-- @
partial' :: Type -> TM Type
partial' = partial_ True

partial_ :: Bool -> Type -> TM Type
partial_ rewrapMaybes ty = do
    nullableFields <- forM (fields ty) $ \field@(b, t) -> case t of
        AppT (ConT w) _ | nameBase w == "Maybe" && not rewrapMaybes -> return field
        _ -> lift $ (b,) <$> [t|Maybe $(return t)|]
    return ty{fields = nullableFields}

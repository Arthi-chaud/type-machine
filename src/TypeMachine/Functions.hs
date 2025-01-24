module TypeMachine.Functions (
    -- * Fields
    pick,
    omit,

    -- * Union and Intersection
    intersection,
    intersection',
    union,
    union',

    -- * Optional
    require,
    partial,
    partial',

    -- * Utils
    keysOf,
)
where

import Control.Monad (forM, forM_)
import Control.Monad.Writer.Strict
import qualified Data.Map.Strict as Map
import Language.Haskell.TH hiding (Type)
import TypeMachine.Internal.Utils
import TypeMachine.Log
import TypeMachine.TM
import TypeMachine.Type

-- | Mark fields are required
--
-- @
--  > data A = A { a :: Maybe Int, b :: Int }
--  > require ["a"] '<:>' 'toType' ''A
--
--  data _ = { a :: Int, b :: Int }
-- @
require :: [String] -> Type -> TM Type
require fieldsNameToRequire ty = return ty{fields = markAsRequired `Map.mapWithKey` fields ty}
  where
    -- TODO Handle any type that is monadplus
    -- TODO Issue warning if type is not optional
    markAsRequired n (b, AppT (ConT p) t)
        | n `elem` fieldsNameToRequire && nameBase p == "Maybe" = (b, t)
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
    forM_ unknownfields $ addLog . fieldNotInType
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
    forM_ unknownfields $ addLog . fieldNotInType
    return ty{fields = removeKeys namesToOmit (fields ty)}
  where
    unknownfields = filter (\f -> not $ f `hasField` ty) namesToOmit

-- | Keep the fields present in both types
--
--  If keys overlap, prefer the type of the left type
--
-- @
--  > data A = A { a :: Int, b :: Int }
--  > data B = B { b :: String, c :: Void }
--  > intersection '<:>' 'toType' ''A  <:> 'toType' ''B
--
--  data _ = { b :: Int }
-- @
intersection :: Type -> Type -> TM Type
intersection a b = return $ a{fields = Map.intersection (fields a) (fields b)}

-- | Keep the fields present in both types
--
--  If keys overlap, prefer the type of the right type
--
-- @
--  > data A = A { a :: Int, b :: Int }
--  > data B = B { b :: String, c :: Void }
--  > intersection' '<:>' 'toType' ''A  <:> 'toType' ''B
--
--  data _ = { b :: String }
-- @
intersection' :: Type -> Type -> TM Type
intersection' = flip intersection

-- | Merge two types together
--
--  If keys overlap, prefer the type of the left type
--
-- @
--  > data A = A { a :: Int, b :: Int }
--  > data B = B { b :: String, c :: Void }
--  > union '<:>' 'toType' ''A  <:> 'toType' ''B
--
--  data _ = { a :: Int, b :: Int, c :: Void }
-- @
union :: Type -> Type -> TM Type
union a b = return $ a{fields = Map.union (fields a) (fields b)}

-- | Merge two types together
--
--  If keys overlap, prefer the type of the right type
--
-- @
--  > data A = A { a :: Int, b :: Int }
--  > data B = B { b :: String, c :: Void }
--  > union '<:>' 'toType' ''A  <:> 'toType' ''B
--
--  data _ = { a :: Int, b :: String, c :: Void }
-- @
union' :: Type -> Type -> TM Type
union' = flip union

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

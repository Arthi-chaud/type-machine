{-# LANGUAGE FlexibleInstances #-}

module TypeMachine.Functions (
    -- * Fields
    pick,
    omit,

    -- * Record
    record,

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

import Control.Monad (forM, when)
import Control.Monad.Writer.Strict
import qualified Data.Foldable as Set
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Language.Haskell.TH hiding (Type, bang)
import qualified Language.Haskell.TH as TH
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
require fieldsNameToRequire ty = do
    logUnknownFields fieldsNameToRequire ty
    updatedFields <- mapM (uncurry markAsRequired) (Map.toList $ fields ty)
    return ty{fields = Map.fromList updatedFields}
  where
    -- TODO Handle any type that is monadplus
    markAsRequired n (b, AppT (ConT p) t)
        | n `elem` fieldsNameToRequire && nameBase p == "Maybe" = return (n, (b, t))
    markAsRequired n r
        | n `elem` fieldsNameToRequire = addLog (fieldNotOptional n) >> return (n, r)
    markAsRequired n r = return (n, r)

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
    logUnknownFields namesToPick ty
    let finalType = ty{fields = keepKeys namesToPick (fields ty)}
    logIfEmptyType finalType
    return finalType

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
    logUnknownFields namesToOmit ty
    return ty{fields = removeKeys namesToOmit (fields ty)}

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
intersection a b = do
    let finalType = a{fields = Map.intersection (fields a) (fields b)}
    logIfEmptyType finalType
    return finalType

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

-- | Creates a type from a list of keys and a 'ToFieldType'
--
-- Issues a log if some keys are duplicated
--
-- @
--  > record ["a", "b"] [t|Int|]
--
--  data _ = { a :: Int, b :: Int }
-- @
record :: [String] -> Q TH.Type -> TM Type
record fNames t = do
    when (Set.length fNameSet /= length fNames) $
        addLog duplicateKey
    when (null fNames) $
        addLog emptyResultType
    fType <- lift t
    return $ Type (mkName "_") (Map.fromSet (const (bang, fType)) fNameSet) []
  where
    bang = Bang NoSourceUnpackedness NoSourceStrictness
    fNameSet = Set.fromList fNames

-- Utils for logs

logUnknownFields :: [String] -> Type -> TM ()
logUnknownFields fieldNames ty =
    addLogs $
        fieldNotInType <$> filter (\fName -> not (fName `hasField` ty)) fieldNames

logIfEmptyType :: Type -> TM ()
logIfEmptyType ty = when (Map.null $ fields ty) $ addLog emptyResultType

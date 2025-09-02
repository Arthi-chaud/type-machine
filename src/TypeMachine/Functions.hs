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

    -- * Type Parameters
    apply,
    applyMany,

    -- * With Selector
    intersectionWithSelector,
    unionWithSelector,
    Selector,

    -- * Utils
    keysOf,
)
where

import Control.Arrow (Arrow (second))
import Control.Monad (foldM, forM, unless, when)
import Control.Monad.Writer.Strict
import qualified Data.Foldable as Set
import Data.Generics
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Language.Haskell.TH hiding (Type, bang)
import qualified Language.Haskell.TH as TH
import TypeMachine.Internal.Utils
import TypeMachine.Log
import TypeMachine.TM
import TypeMachine.Type
import Prelude hiding (Either (..))

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
    failIfHasTypeVariables ty
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
--  > omit ["b", "c"] '<:>' 'toType' ''A
--
--  data _ = { a :: Int }
-- @
omit :: [String] -> Type -> TM Type
omit namesToOmit ty = do
    failIfHasTypeVariables ty
    logUnknownFields namesToOmit ty
    let resultType = ty{fields = removeKeys namesToOmit (fields ty)}
    logIfEmptyType resultType
    return resultType

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
intersection = intersectionWithSelector (const Left)

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
intersection' = intersectionWithSelector (const Right)
{-# INLINE intersection' #-}

-- | Variant of 'intersection' where user can decide which value to keep (the left object's or the right's) in case of overlap
intersectionWithSelector :: (String -> Selector) -> Type -> Type -> TM Type
intersectionWithSelector select a b = do
    failIfHasTypeVariables a
    failIfHasTypeVariables b
    let finalType =
            a
                { fields =
                    Map.intersectionWithKey
                        (\key aField bField -> if select key == Left then aField else bField)
                        (fields a)
                        (fields b)
                }
    logIfEmptyType finalType
    return finalType

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
union = unionWithSelector (const Left)

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
union' = unionWithSelector (const Right)
{-# INLINE union' #-}

-- | Variant of 'union' where user can decide which value to keep (the left object's or the right's) in case of overlap
unionWithSelector :: (String -> Selector) -> Type -> Type -> TM Type
unionWithSelector select a b = do
    failIfHasTypeVariables a
    failIfHasTypeVariables b
    return $
        a
            { fields =
                Map.unionWithKey
                    (\key aField bField -> if select key == Left then aField else bField)
                    (fields a)
                    (fields b)
            }

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

-- | Replaces the first type parameter with the given type
--
-- Issues a warning if there are no type variable
--
-- @
--
--  data A x = { a :: Maybe x }
--
--  > 'apply' [t|Int|] \<:\> 'toType' ''A
--
--  data _ = { a :: Maybe Int }
-- @
apply :: Q TH.Type -> Type -> TM Type
apply _ ty@(Type _ _ []) = addLogs [noTypeParameter] >> return ty
apply qt ty@(Type _ f ((tp, _) : tps)) = do
    typeParameterValue <- lift qt
    let fieldsWithTypeApplied = second (replaceTypeVar tp typeParameterValue) <$> f
    return ty{fields = fieldsWithTypeApplied, typeParams = tps}
  where
    replaceTypeVar varName value src =
        let
            mapper t@(VarT n) =
                if nameBase n == varName
                    then value
                    else t
            mapper t = t
         in
            everywhere (mkT mapper) src

-- | Applies multiple type arguments
--
-- Issues a warning if too many arguments are given
--
-- @
--
--  data A a b = { a :: Maybe a, b :: b }
--
--  > 'applyMany' ([t|Int|], [t|String|]) \<:\> 'toType' ''A
--
--  data _ = { a :: Maybe Int, b :: String }
-- @
applyMany :: [Q TH.Type] -> Type -> TM Type
applyMany typeArgs ty = foldM (flip apply) ty typeArgs

-- | Selector for functions like 'intersectionWithSelector'
data Selector = Left | Right deriving (Eq, Show)

-- Utils for logs

logUnknownFields :: [String] -> Type -> TM ()
logUnknownFields fieldNames ty =
    addLogs $
        fieldNotInType <$> filter (\fName -> not (fName `hasField` ty)) fieldNames

logIfEmptyType :: Type -> TM ()
logIfEmptyType ty = when (Map.null $ fields ty) $ addLog emptyResultType

failIfHasTypeVariables :: Type -> TM ()
failIfHasTypeVariables ty =
    unless (null $ typeParams ty) $
        fail "Warning - The behaviour of this function is not tested on types with type parameters."

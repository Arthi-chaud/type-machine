module TypeMachine.Internal.Utils (removeKeys, keepKeys) where

import qualified Data.Map.Strict as Map

removeKeys :: (Eq k) => [k] -> Map.Map k v -> Map.Map k v
removeKeys keys = Map.filterWithKey (\k _ -> k `notElem` keys)

keepKeys :: (Eq k) => [k] -> Map.Map k v -> Map.Map k v
keepKeys keys = Map.filterWithKey (\k _ -> k `elem` keys)

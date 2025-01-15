module TypeMachine.TH.Internal.Utils (capitalize, getRecordConstructorVars) where

import Data.Char (toUpper)
import Language.Haskell.TH hiding (Type, reifyType)

-- | Capitalize the first letter of a string
capitalize :: String -> String
capitalize (c : cs) = toUpper c : cs
capitalize cs = cs

-- From a 'DataD''s constructors, returns its unique constructor's fields.
--
-- Fails if the list of 'Con' does not have exactly one record constructor
getRecordConstructorVars :: (MonadFail m) => [Con] -> m [VarBangType]
getRecordConstructorVars [RecC _ vbt] = return vbt
getRecordConstructorVars _ = fail "Type-Machine Error: Expected type to have exactly one Record constructor"

-- TODO I don't like passing [Con] as parameter. Might need to redesign this function

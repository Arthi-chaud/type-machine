module TypeMachine.TypeFunction where

import Control.Monad.Writer.Lazy
import Language.Haskell.TH hiding (Type)
import TypeMachine.TH.Internal.Type

type TypeMachineLog = String

-- | A 'TypeFunction' is a function that takes a 'Type' as parameter and can:
--
-- - Emit warning messages (e.g. when omitting that does not exist)
-- - Take advantage of the 'Language.Haskell.TH.Q' monad's features
type TypeFunction a = Type -> WriterT [TypeMachineLog] Q a

-- | Execute a 'TypeFunction' and issue logs
runTypeFunction :: Type -> TypeFunction a -> Q a
runTypeFunction t f = do
    (res, _logs) <- runWriterT (f t)
    -- TODO Format and format logs
    return res

remove :: String -> TypeFunction Type
remove nameToRemove ty = return ty{fields = filteredFields}
  where
    -- TODO: Warning if field does not exist
    filteredFields = filter (\(n, _, _) -> nameBase n /= nameToRemove) $ fields ty

require :: String -> TypeFunction Type
require fieldNameToRequire ty = return ty{fields = markAsRequired <$> fields ty}
  where
    -- TODO Handle any type that is monadplus
    markAsRequired (n, b, AppT (ConT p) t)
        | fieldNameToRequire == nameBase n && nameBase p == "Maybe" = (n, b, t)
    markAsRequired r = r

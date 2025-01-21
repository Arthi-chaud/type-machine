module TypeMachine.TM (
    TM,
    runTM,
    toType,
) where

import Control.Monad (forM_)
import Control.Monad.Writer.Lazy
import Language.Haskell.TH hiding (Type, reifyType)
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

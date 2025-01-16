module TypeMachine.Log (TypeMachineLog, formatLog) where

type TypeMachineLog = String

-- | Format a log message, to be printed to the user
formatLog :: TypeMachineLog -> String
formatLog = (++) "TypeMachine: "

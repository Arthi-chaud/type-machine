{-# LANGUAGE GADTs #-}

module TypeMachine.Log (
    TypeMachineLog,

    -- * Common Log messages
    fieldNotInType,
    emptyResultType,
    fieldNotOptional,

    -- * Formatting
    formatLog,
) where

type TypeMachineLog = String

-- | Format a log message, to be printed to the user
formatLog :: TypeMachineLog -> String
formatLog = (++) "TypeMachine: "

fieldNotInType :: String -> TypeMachineLog
fieldNotInType f = "Field '" ++ f ++ "' not in type."

emptyResultType :: TypeMachineLog
emptyResultType = "Result type is empty."

fieldNotOptional :: String -> TypeMachineLog
fieldNotOptional f = "Field '" ++ f ++ "' is not optional."

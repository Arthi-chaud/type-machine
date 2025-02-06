{-# LANGUAGE GADTs #-}

module TypeMachine.Log (
    TypeMachineLog,

    -- * Common Log messages
    fieldNotInType,
    emptyResultType,
    fieldNotOptional,
    duplicateKey,
    noTypeParameter,

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

noTypeParameter :: TypeMachineLog
noTypeParameter = "There are no type parameters in this type."

duplicateKey :: TypeMachineLog
duplicateKey = "Some keys are duplicated."

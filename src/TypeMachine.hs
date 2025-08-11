module TypeMachine (
    -- * Entrypoint
    type_,

    -- * Main Type Functions
    module TypeMachine.Functions,

    -- * Is
    defineIs,
    deriveIs,

    -- * Infixes
    (<:>),
    (<::>),

    -- * Internal types
    Type,
    TM,

    -- * Log types
) where

import TypeMachine.Functions
import TypeMachine.TH
import TypeMachine.TM
import TypeMachine.TM.Syntax
import TypeMachine.Type

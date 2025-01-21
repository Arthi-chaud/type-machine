module TypeMachine (
    -- * Entrypoint
    type_,

    -- * Main type functions
    module TypeMachine.Functions,

    -- * Infixes
    (<:>),
    (<::>),
    (<.>),

    -- * Internal types
    Type,
    TM,
) where

import TypeMachine.Functions
import TypeMachine.TH
import TypeMachine.TM
import TypeMachine.TM.Syntax
import TypeMachine.Type

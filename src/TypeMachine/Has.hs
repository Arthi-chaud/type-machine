{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TypeMachine.Has (Has (..)) where

import GHC.Base

-- | typeclass inspired by superrecord to enfore that a given record type has a field of a specific type
class Has (label :: Symbol) a ty where
    get :: a -> ty
    set :: ty -> a -> a

instance (Alternative m) => Has label a (m ty) where
    get _ = empty
    set _ a = a

instance (Monoid ty) => Has label a ty where
    get _ = mempty
    set _ a = a

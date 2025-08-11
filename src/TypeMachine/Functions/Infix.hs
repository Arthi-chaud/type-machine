module TypeMachine.Functions.Infix (
    -- * Intersection
    (<#|>),
    (<:#|>),
    (<#|:>),

    -- ** Flipped Intersection
    (<|#>),
    (<:|#>),
    (<|#:>),

    -- * Union
    (<#&>),
    (<:#&>),
    (<#&:>),

    -- ** Flipped Union
    (<&#>),
    (<:&#>),
    (<&#:>),
) where

import Language.Haskell.TH (Name)
import TypeMachine.Functions
import TypeMachine.TM
import TypeMachine.Type

-- | Alias to 'intersection'
(<#|>) :: Type -> Type -> TM Type
(<#|>) = intersection
{-# INLINE (<#|>) #-}

-- | Like '<#|>', but the first parameter is a name
(<:#|>) :: Name -> Type -> TM Type
a <:#|> b = toType a >>= \a' -> a' <#|> b

-- | Like '<#|>', but the second parameter is a name
(<#|:>) :: Type -> Name -> TM Type
a <#|:> b = toType b >>= \b' -> a <#|> b'

-- | Alias to 'intersection''
(<|#>) :: Type -> Type -> TM Type
(<|#>) = intersection'
{-# INLINE (<|#>) #-}

-- | Like '<|#>', but the first parameter is a name
(<:|#>) :: Name -> Type -> TM Type
a <:|#> b = toType a >>= \a' -> a' <|#> b

-- | Like '<|#>', but the second parameter is a name
(<|#:>) :: Type -> Name -> TM Type
a <|#:> b = toType b >>= \b' -> a <|#> b'

-- | Alias to 'union'
(<#&>) :: Type -> Type -> TM Type
(<#&>) = union
{-# INLINE (<#&>) #-}

-- | Like '<#&>', but the first parameter is a name
(<:#&>) :: Name -> Type -> TM Type
a <:#&> b = toType a >>= \a' -> a' <#&> b

-- | Like '<#&>', but the second parameter is a name
(<#&:>) :: Type -> Name -> TM Type
a <#&:> b = toType b >>= \b' -> a <#&> b'

-- | Alias to 'union''
(<&#>) :: Type -> Type -> TM Type
(<&#>) = union'
{-# INLINE (<&#>) #-}

-- | Like '<&#>', but the first parameter is a name
(<:&#>) :: Name -> Type -> TM Type
a <:&#> b = toType a >>= \a' -> a' <&#> b

-- | Like '<&#>', but the second parameter is a name
(<&#:>) :: Type -> Name -> TM Type
a <&#:> b = toType b >>= \b' -> a <&#> b'

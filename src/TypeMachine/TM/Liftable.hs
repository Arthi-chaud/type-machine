{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module TypeMachine.TM.Liftable (LiftableTMFunction (applyTM)) where

import TypeMachine.TM

class LiftableTMFunction f where
    applyTM :: forall a b. (f ~ (a -> b)) => (a -> b) -> TM a -> b

instance LiftableTMFunction (a -> TM b) where
    applyTM f v = v >>= f

instance LiftableTMFunction (a -> b -> TM c) where
    applyTM f ma b = do
        a <- ma
        f a b

instance LiftableTMFunction (a -> b -> c -> TM d) where
    applyTM f ma b c = do
        a <- ma
        f a b c

instance LiftableTMFunction (a -> b -> c -> d -> TM e) where
    applyTM f ma b c d = do
        a <- ma
        f a b c d

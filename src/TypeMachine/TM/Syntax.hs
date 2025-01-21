{-# LANGUAGE FlexibleContexts #-}

module TypeMachine.TM.Syntax ((<:>), (<.>)) where

import TypeMachine.TM
import TypeMachine.TM.Liftable

-- | Apply a `TM a` value to a 'TM' computation that expects an `a`
--
-- Example:
--
-- @
--  'omit' ["id"] '<:>' 'toType' ''User
-- @
(<:>) :: (LiftableTMFunction (a -> b)) => (a -> b) -> TM a -> b
(<:>) = applyTM

-- | Apply a `a` value to a 'TM' computation that expects an `a`.
--
-- It is just an application operator. It exists so that applications of 'TM' functions is visually homogeneous
-- Not using it when `a` is the first parameter can enhance readability.
--
-- Examples:
--
-- @
--  'omit' '<.>' ["id"] '<:>' 'toType' ''User
--  -- Is equivalent to
--  'omit' ["id"] '<:>' 'toType' ''User
-- @
--
-- If the parameters to this function were flipped, using '<.>' can be handy:
--
-- @
--  'flip' 'omit' '<:>' 'toType' ''User '<.>' ["id"]
--  -- Instead of having to use parenthesis
--  ('flip' 'omit' '<:>' 'toType' ''User) ["id"]
--  -- Or the application operator:
--  'flip' 'omit' '<:>' 'toType' ''User $ ["id"]
-- @
(<.>) :: (a -> b) -> a -> b
f <.> v = f v

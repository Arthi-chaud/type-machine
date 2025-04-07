{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import TypeMachine

$(type_ "Vector2" (record ["x", "y"] [t|Int|]))
$(defineIs ''Vector2)

$(type_ "Vector3" (union <::> ''Vector2 <:> record ["z"] [t|Int|]))
$(deriveIs ''Vector2 ''Vector3)

translateX :: (IsVector2 a) => Int -> a -> a
translateX n v = setX (n + getX v) v

main :: IO ()
main = return ()

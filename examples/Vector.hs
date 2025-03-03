{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

import TypeMachine

$(type_ "Vector2" (record ["x", "y"] [t|Int|]))
$(defineIs ''Vector2)

translate :: (IsVector2 a) => a -> a
translate = undefined

$(type_ "Vector3" ((union <::> ''Vector2) =<< record ["z"] [t|Int|]))
$(deriveIs ''Vector2 ''Vector3)

main :: IO ()
main = return ()

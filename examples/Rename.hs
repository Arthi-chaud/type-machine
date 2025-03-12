{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Data.Map
import TypeMachine
import TypeMachine.Type

rename :: (String -> String) -> Type -> TM Type
rename f t = return (t{fields = renamedFields})
  where
    renamedFields = mapKeys f $ fields t

$(type_ "Point" (record ["x", "y"] [t|Int|]))

$(type_ "Centroid" (rename ("centroid_" ++) <::> ''Point))

main :: IO ()
main = return ()

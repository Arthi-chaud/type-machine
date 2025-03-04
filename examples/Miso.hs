{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Char
import qualified Data.Map as M
import Miso hiding (type_)
import Miso.String hiding (concatMap, toLower)
import Text.Printf
import TypeMachine

data Color = Blue | White | Red deriving (Show)

data Size = XS | SM | MD | LG | XL

toPixel :: Size -> Int
toPixel s = case s of
    XS -> 5
    SM -> 10
    MD -> 15
    LG -> 20
    XL -> 25

data ButtonProps = ButtonProps
    { label :: String
    , color :: Color
    , size :: Size
    }

button :: ButtonProps -> View a
button (ButtonProps l c s) =
    button_
        [style_ $ M.fromList [("color", pack $ toLower <$> show c), ("padding", pack $ printf "%spx" $ toPixel s)]]
        [text $ pack l]

$(type_ "ButtonGridProps" (pick ["label"] <::> ''ButtonProps))

buttonGrid :: [ButtonGridProps] -> View a
buttonGrid props = div_ [] $ (\(ButtonGridProps l) -> button $ ButtonProps l White LG) <$> props

main :: IO ()
main = startApp App{..}
  where
    initialAction = ()
    model = ()
    update _ _ = undefined
    view () = viewModel
    events = defaultEvents
    subs = []
    mountPoint = Nothing
    logLevel = Off

viewModel :: View ()
viewModel =
    div_
        []
        [buttonGrid (ButtonGridProps <$> ["Hello", "Goodbye"])]

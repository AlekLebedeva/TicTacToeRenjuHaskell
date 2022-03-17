
module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color ( makeColor )
import Graphics.Gloss.Interface.Pure.Game

import Game
import Graphics.Gloss (black)

window = InWindow "Tic Tac Toe and Renju" (screenWidth , screenHeight ) (200, 210)
backgroundColor = makeColor 126 150 168 255

main :: IO ()
main = play window black 30 initialGame gameAsPicture transformGame (const id)
    
    {-do
    display window backgroundColor drawing
    where
        drawing = Pictures [Circle 90]-}
    --play window backgroundColor 30 initialGame gameAsPicture transformGame (const id)
    --(EventKey (SpecialKey KeyUp) Down _ _)  = play windowtic backgroundColor 30 initialGametic gameAsPicturetic transformGametic (const id)
--handleKeys (EventKey (SpecialKey KeyUp) Down _ _)  = play windowtic backgroundColor 30 initialGametic gameAsPicturetic transformGametic (const id)
--main = play window backgroundColor 30 initialGame gameAsPicture transformGame (const id)
{-main = display window backgroundColor drawing
    where
        drawing = Pictures [Circle 90,
                            Text "Start TicTacToe"]-}
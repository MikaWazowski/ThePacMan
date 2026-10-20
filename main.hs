
module Main where

import Game.Logic
import Game.Types (Direction(East))

main :: IO()
main = print (stepPosition East (1, 2))

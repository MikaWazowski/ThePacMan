
module Main where

import Game.Logic
import Game.Types

main :: IO ()
main = do 
    putStrLn ("Start:              " ++ show (position pac)) 
    putStrLn ("East into wall:     " ++ show (position blocked))
    putStrLn ("North into open tile: " ++ show (position open))

  where
    board = Board [Wall 1 (2, 3)] 10
    pac = PacMan (1, 3) North Nothing
    blocked = movePacMan board East pac
    open = movePacMan board North pac

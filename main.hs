module Main where

import Game.Controller
import Game.Render
import Game.Types
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = play
  (InWindow "Pac-Man" (640, 640) (100, 500))  --window
  black                                       --background color
  30                                          --updates ps
  initialState                                                   
  render          
  handleInput
  step

-- Time-based part always needed by play but isn't implemented yet.
step :: Float -> GameState -> GameState
step _ state = state

initialState :: GameState
initialState = GameState
  { board = Board [Wall 4 (3, 3), Wall 3 (2, 7)] 10
  , pacMan = PacMan (1, 1) East East
  , ghost = []
  , dots = []
  , score = 0
  , lives = 3
  , statusGame = Ongoing
  , timerOngoing = 0
  }

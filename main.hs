module Main where
import Game.Update
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

initialState :: GameState
initialState = GameState
  { board = Board [Wall 4 (3, 3), Wall 3 (2, 7)] 10
  , pacMan = PacMan (1, 1) East East
  , ghost = [(Ghost Blinky (3, 3) Normal North),(Ghost Pinky (3, 3) Normal North), (Ghost Clyde (7, 7) Normal North) , (Ghost Inky (9, 3) Normal North)  ]
  , dots = []
  , score = 0
  , lives = 3
  , statusGame = Ongoing
  , timerOngoing = 0
  }

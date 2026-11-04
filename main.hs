module Main where
import Game.Update
import Game.Controller
import Game.InitialState
import Game.Render
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

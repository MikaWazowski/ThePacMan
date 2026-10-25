module Game.Controller where

import Game.Logic (movePacMan)
import Game.Types
import Graphics.Gloss.Interface.Pure.Game

handleInput :: Event -> GameState -> GameState

-- EventKey key keyState modifier mousePosition

-- You can either move pacman with WASD or with the arrow buttons
handleInput (EventKey (Char 'w') Down _ _) state = move North state
handleInput (EventKey (Char 's') Down _ _) state = move South state
handleInput (EventKey (Char 'a') Down _ _) state = move West state
handleInput (EventKey (Char 'd') Down _ _) state = move East state


handleInput (EventKey (SpecialKey KeyUp) Down _ _) state = move North state
handleInput (EventKey (SpecialKey KeyDown) Down _ _) state = move South state
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) state = move West state
handleInput (EventKey (SpecialKey KeyRight) Down _ _) state = move East state

-- If the user didn't press any of the buttons above, nothing happens
handleInput _ state = state

move :: Direction -> GameState -> GameState
move direction state = state { pacMan = movePacMan (board state) direction (pacMan state) }

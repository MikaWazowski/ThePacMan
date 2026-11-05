module Main where
import Game.Update
import Game.Controller
import Game.InitialState
import Game.Render
import Game.Types
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Directory

main :: IO ()
main = playIO
  (InWindow "Pac-Man" (640, 640) (100, 500))  --window
  black                                       --background color
  30                                          --updates ps
  initialState                                                   
  renderIO
  handleInputIO
  stepAndSave

renderIO :: GameState -> IO Picture
renderIO state = pure (render state)

handleInputIO :: Event -> GameState -> IO GameState
handleInputIO event state = pure (handleInput event state)

stepAndSave :: Float -> GameState -> IO GameState
stepAndSave time state
  -- save only when the game changes from ongoing to lost not every step after game over.
  | statusGame state == Ongoing && statusGame nextState == Lost = do
      saveHighscore (score nextState)
      pure nextState
  | otherwise = pure nextState
  where nextState = step time state

saveHighscore :: Int -> IO ()
saveHighscore currentScore = do
  exists <- doesFileExist "highscore.txt"
  previous <- readPreviousScore exists
  writeHighscore currentScore (read previous)

readPreviousScore :: Bool -> IO String
readPreviousScore exists
  | exists = readFile "highscore.txt"
  | otherwise = pure "0"

writeHighscore :: Int -> Int -> IO ()
writeHighscore currentScore highscore
  | currentScore > highscore = writeFile "highscore.txt" (show currentScore)
  | otherwise = pure ()

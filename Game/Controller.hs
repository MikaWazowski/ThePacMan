module Game.Controller where

import Game.Types
import Game.InitialState
import Graphics.Gloss.Interface.Pure.Game
import Game.Update
import System.IO.Unsafe (unsafePerformIO)
handleInput :: Event -> GameState -> GameState

-- EventKey key keyState modifier mousePosition

-- You can either move pacman with WASD or with the arrow buttons
handleInput (EventKey (SpecialKey KeySpace) Down _ _) state
  | statusGame state == Lost = initialState { board = board state }
  | statusGame state == Paused = state { statusGame = Ongoing }

handleInput (EventKey (Char 'p') Down _ _) state = state {statusGame = Paused}
handleInput (EventKey (Char 'h') Down _ _) state = state {statusGame = Home}
handleInput (EventKey (Char 'w') Down _ _) state = move North state
handleInput (EventKey (Char 's') Down _ _) state = move South state
handleInput (EventKey (Char 'c') Down _ _) state = state {statusGame = Custom}
handleInput (EventKey (Char 'o') Down _ _) state = unsafePerformIO (openGame state "walls.txt" )
handleInput (EventKey (Char 'e') Down _ _) state = unsafePerformIO (saveGame state )
handleInput (EventKey (Char 'a') Down _ _) state = move West state
handleInput (EventKey (Char 'd') Down _ _) state = move East state


handleInput (EventKey (SpecialKey KeyUp) Down _ _) state = move North state
handleInput (EventKey (SpecialKey KeyDown) Down _ _) state = move South state
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) state = move West state
handleInput (EventKey (SpecialKey KeyRight) Down _ _) state = move East state

handleInput (EventKey (MouseButton LeftButton) Down _ (x, y)) state = updatePacMan 0 state True( wallmaker state x y )
handleInput (EventKey (MouseButton RightButton) Down _ (x, y)) state = updatePacMan 0 state True( removeWall state x y )

-- If the user didn't press any of the buttons above, nothing happens
handleInput _ state = state

saveGame state = do 
    let content = gameToString state
    writeFile "walls.txt" content
  -- hier kun je wallLines gebruiken (:: [String])
    let newState = state 
    return newState 

gameToString :: GameState -> String
gameToString state = stringState where
  stringState = wallsToString (walls (board state))

wallsToString:: [Wall] -> String
wallsToString [] = ""
wallsToString(x:xs) = (show (fst wallPositie)) ++ " " ++ (show (snd wallPositie)) ++ "\n" ++ wallsToString xs where
  wallPositie = startPosition x

openGame :: GameState -> String ->  IO GameState
openGame state path = do
  wallLines <- lines <$> readFile path
  -- hier kun je wallLines gebruiken (:: [String])
  let newBoard = (board state) {walls = createWallList state wallLines}
  let newState = state {board = newBoard}
  return newState 
    

createWallList :: GameState -> [String] -> [Wall]
createWallList state [] = []
createWallList state (x:xs) = Wall 1 (round xWall, round yWall) : createWallList state xs where
  positie = getPosition (words x)
  xWall = fst positie
  yWall = snd positie

getPosition :: [String] -> (Float, Float)
getPosition (x:y:ys) = (read  x, read y)

wallmaker :: GameState -> Float -> Float -> [Wall] 
wallmaker state x y = wall:(walls(board state)) where
  wall =Wall 1 (coorToCell x y ) 
  --  | x > y =
  --  | x > y =Wall 1 (9,9) 
  --  | otherwise =Wall 1 (0,0)

removeWall :: GameState -> Float -> Float -> [Wall]
removeWall state x y = wall:(walls(board state)) where
  wall =Wall 1 (coorToCell x y ) 


coorToCell :: Float -> Float -> Position
coorToCell x y = ( naarCel x, naarCel y)

naarCel :: Float -> Int
naarCel x = floor ((x + 8*32) / 32)

move :: Direction -> GameState -> GameState
move direction state
  | statusGame state /= Ongoing = state
  | otherwise = state { pacMan = (pacMan state) { directionWant = direction } }

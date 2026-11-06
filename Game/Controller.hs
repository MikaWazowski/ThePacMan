module Game.Controller where

import Game.InitialState
import Game.Types
import Game.Update
import Graphics.Gloss.Interface.Pure.Game
import System.Directory
import System.IO.Unsafe (unsafePerformIO)

handleInput :: Event -> GameState -> GameState
-- EventKey key keyState modifier mousePosition

-- You can either move pacman with WASD or with the arrow buttons
handleInput (EventKey (SpecialKey KeySpace) Down _ _) state
  | statusGame state == Lost = initialState {board = board state}
  | statusGame state == Paused = state {statusGame = Ongoing}
handleInput (EventKey (Char 'p') Down _ _) state = state {statusGame = Paused}
handleInput (EventKey (Char 'h') Down _ _) state = state {statusGame = Home}
handleInput (EventKey (Char 'w') Down _ _) state = move North state
handleInput (EventKey (Char 's') Down _ _) state = move South state
handleInput (EventKey (Char 'a') Down _ _) state = move West state
handleInput (EventKey (Char 'd') Down _ _) state = move East state

handleInput (EventKey (SpecialKey KeyUp) Down _ _) state = move North state
handleInput (EventKey (SpecialKey KeyDown) Down _ _) state = move South state
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) state = move West state
handleInput (EventKey (SpecialKey KeyRight) Down _ _) state = move East state

--Open the custom level Menu when C is pressed. 
handleInput (EventKey (Char 'c') Down _ _) state = state {statusGame = Custom}
--Saving the game when E for Export is pressed. 
handleInput (EventKey (Char 'e') Down _ _) state = unsafePerformIO (saveGame state)
--Opening one of the 5 custom games. 
handleInput (EventKey (Char '1') Down _ _) state = unsafePerformIO (openGame state "walls.txt" 1)
handleInput (EventKey (Char '2') Down _ _) state = unsafePerformIO (openGame state "walls.txt" 2)
handleInput (EventKey (Char '3') Down _ _) state = unsafePerformIO (openGame state "walls.txt" 3)
handleInput (EventKey (Char '4') Down _ _) state = unsafePerformIO (openGame state "walls.txt" 4)
handleInput (EventKey (Char '5') Down _ _) state = unsafePerformIO (openGame state "walls.txt" 5)

--Placing walls and power pellets when left- of right mousebutton is pressed. 
handleInput (EventKey (MouseButton LeftButton) Down _ (x, y)) state = updatePacMan 0 state True (wallmaker state x y) (dots state)
handleInput (EventKey (MouseButton RightButton) Down _ (x, y)) state = updatePacMan 0 state True (walls (board state)) (placePellet state x y)
-- If the user didn't press any of the buttons above, nothing happens
handleInput _ state = state

--Saving the gamestate to a string and returning the provided gamestate. 
saveGame :: GameState -> IO GameState
saveGame state = do
  --Getting the filename for saving the gamestate. 
  let content = gameToString state
  name <- nameFile 0
  --Writing the gamestate to the file. 
  writeFile name content
  let newState = state
  return newState


--Function for placing a Power Pellet on a specific position. 
placePellet :: GameState -> Float -> Float -> [Dot]
placePellet state x y = dot : (dots state)
  where
    dot = Dot False Big (coorToCell x y)

--This is the function for naming the file. It checks if there already is a file with the same name, in which case the name will be walls +count +1
nameFile :: Int -> IO String
nameFile count = do
  let name = "walls" ++ show count ++ ".txt"
  --Checking if the file already exists. 
  exists <- doesFileExist name
  if exists
    then nameFile (count + 1)
    else return name

--Converting the gamestate to a long String. 
gameToString :: GameState -> String
gameToString state = stringState
  where
    --            Every wall in a String form.           Every Dot in String form. 
    stringState = wallsToString (walls (board state)) ++ dotsToString (filter (\d -> dotType d == Big) (dots state))

--Converting every wall to a String. 
wallsToString :: [Wall] -> String
--Recursive base case. 
wallsToString [] = ""
--Take every wall, converting is, and placing it at the front of the other walls in string form. 
wallsToString (x : xs) = "w " ++ (show (fst wallPositie)) ++ " " ++ (show (snd wallPositie)) ++ "\n" ++ wallsToString xs
  where
    wallPositie = startPosition x

--Converting every dot to a String
dotsToString :: [Dot] -> String
--Recursive base case. 
dotsToString [] = ""
--Take every dot, converting is, and placing it at the front of the other dot in string form. 
dotsToString (x : xs) = "d " ++ (show (fst dotPos)) ++ " " ++ (show (snd dotPos)) ++ "\n" ++ dotsToString xs
  where
    dotPos = dotPosition x

--Opening a file and returning a gamestate with every dot and wall in the save file. 
openGame :: GameState -> String -> Int -> IO GameState
openGame state path count = do
  let name = "walls" ++ (show count) ++ ".txt"
  --If the file exists the read is, otherwise return nothing. 
  exist <- doesFileExist name
  wallLines <-
    if exist
      then
        lines <$> readFile name
      else return []
  --Defining the new board and state. 
  let newBoard = (board state) {walls = createWallList state wallLines}
  let newState = state {board = newBoard, dots = (createDotList state wallLines)}
  return newState

--Creating a list of walls based on a list of strings. 
createWallList :: GameState -> [String] -> [Wall]
--Defining recursive base case. 
createWallList state [] = []
--Converting every line in the file (which is a single wall or dot.) to a wall. 
createWallList state (x : xs) =
  let positie = getPosition (words x)
      wallCheck = getFirst positie
      xWall = getSecond positie
      yWall = getThird positie
      --Place wall in the list if it is definied as a wall in the file. 
   in if wallCheck == "w"
        then
          Wall 1 (round xWall, round yWall) : createWallList state xs
        else createWallList state xs

--Creating a list of walls based on a list of strings. 
createDotList :: GameState -> [String] -> [Dot]
--Defining recursive base case. 
createDotList state [] = []
--Converting every line in the file (which is a single wall or dot.) to a dot
createDotList state (x : xs) = 
  let positie = getPosition (words x)
      wallCheck = getFirst positie
      xWall = getSecond positie
      yWall = getThird positie
            --Place dot in the list if it is definied as a dot in the file. 
   in if wallCheck == "d"
        then
          Dot False Big (round xWall, round yWall) : createDotList state xs
        else createDotList state xs

--Get first of a tripple tuple. 
getFirst :: (a, b, c) -> a
getFirst (x, _, _) = x

--Get second of a tripple tuple. 
getSecond :: (a, b, c) -> b
getSecond (_, y, _) = y

--Get third of a tripple tuple. 
getThird :: (a, b, c) -> c
getThird (_, _, x) = x

--get the position and wall/dot check from a line of strings. 
getPosition :: [String] -> (String, Float, Float)
getPosition (x : y : z : ys) = (x, read y, read z)

--Place a wall on a single cell
wallmaker :: GameState -> Float -> Float -> [Wall]
wallmaker state x y = wall : (walls (board state))
  where
    wall = Wall 1 (coorToCell x y)

--Convert two coordinate floats to a cell position. 
coorToCell :: Float -> Float -> Position
coorToCell x y = (naarCel x, naarCel y)

--Convert a coordinate float to a cell position. 
naarCel :: Float -> Int
naarCel x = floor ((x + 8 * 32) / 32)

--Move pacman in the direction stated. 
move :: Direction -> GameState -> GameState
move direction state
  | statusGame state /= Ongoing = state
  | otherwise = state {pacMan = (pacMan state) {directionWant = direction}}

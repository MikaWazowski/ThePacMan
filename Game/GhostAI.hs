module Game.GhostAI where

import Game.Types
import System.Random (StdGen, randomR, randomRIO)
import System.IO.Unsafe (unsafePerformIO)


--This function takes two positions and calculates the direction the line between those two point would go from the first position to the second. 
positionToDirection2:: Position -> Position -> Direction
positionToDirection2 (x, y) (x2, y2) 
  |(x > x2) = West
  |(x < x2) = East
  |(y > y2) = South
  |(y < y2) = North

--This function checks if the field is walkable, meaning is it still within bounderies, and is there a wall in that place. 
fieldIsWalkable :: Board -> Position -> Bool
fieldIsWalkable b (x, y) =
  x >= 0
    && y >= 0
    && x < boardDimensions b
    && y < boardDimensions b
    && not (any wallPlace (walls b))
  where
    wallPlace w =
      let (wx, wy) = startPosition w
       in y == wy && x >= wx && x < wx + Game.Types.lengthWall w

--This function calculates the shortest path to a position (PacMan) and gives the first direction you need to go to achieve that position. 
routes :: Board -> Position -> [(Position, Direction)] -> [Position] -> Direction
--             ^ target       ^ queue                    ^ visited
--Base Case, if there is no way to the position, just go north. 
routes _ _ [] _ = North
--For each neighboring cell, which is not a wall or outside the bounds of the game. It is checked if it is on the target position. 
--If not, all the neighbours of the neighbouring cells are checked recursively. 
routes gameBoard (xTarget, yTarget) (((xNeighbor, yNeighbor), neighborDirection) : xs) visited = winner where
     winner
      |(xTarget, yTarget) == (xNeighbor, yNeighbor) = neighborDirection
      |otherwise = routes gameBoard (xTarget, yTarget) (xs ++ new) (newNeighbors ++ visited)
      where 
      newNeighbors = neighbours gameBoard (xNeighbor, yNeighbor) [(0,1),(1,0),(0,-1),(-1,0)] visited
      new = [ (n, neighborDirection) | n <- newNeighbors ]

--The function that returns the positions of every neighbour that is not a wall, or outside the bounds of the game
neighbours :: Board -> Position -> [Position] -> [Position] -> [Position]
neighbours gameBoard position [] visited = []
neighbours gameBoard (x, y) ((xcheck, ycheck) : xs) visited
  | fieldIsWalkable gameBoard (x + xcheck, y + ycheck) && (notElem (x + xcheck, y + ycheck) visited)= (x + xcheck, y + ycheck) : neighbours gameBoard (x, y) xs visited
  | otherwise = neighbours gameBoard (x, y) xs visited

--This is the function that calculates the direction the ghost need to go to go after PacMan or run away if they are afraid. It uses the explained above functions. 
--This is also the function the ghost Blinky uses, he just goes after PacMan directly. 
closestPoint :: Board -> Ghost -> Position -> Position -> Direction -> Direction
closestPoint gameBoard g target ghostPosition currentDirection
  | target == ghostPosition = currentDirection
  | null start = currentDirection
  | status == Afraid = unsafePerformIO (randomDirIO currentDirection)
  | otherwise = routes gameBoard target start (ghostPosition : map fst start)
  where
    start = [ (n, positionToDirection2 ghostPosition n)
            | n <- neighbours gameBoard ghostPosition [(0,1),(1,0),(0,-1),(-1,0)] [ghostPosition] ]
    status = statusGhost g

--A function needed to calculate the opposite direction. 
oppositeDir :: Direction -> Direction
oppositeDir North = South
oppositeDir South = North
oppositeDir West = East
oppositeDir East = West

--When the ghosts are scared, they use this function to scatter in random directions, as long as it is not a complete 180 degree turn. 
randomDirIO :: Direction -> IO Direction
randomDirIO dir = do
  let options = filter (/= oppositeDir dir) [North, East, South, West]
  i <- randomRIO (0, length options - 1)
  return (options !! i)

--If the target is outside the bounds of the map, this function makes sure that the new target is set inside the bounds of the map. 
--It does this by recursivly removing one step from the direction it is aimed until it is in the map again. 
toWalkable :: Board -> Position -> Direction -> Position
toWalkable b (x, y) direction
  | not (fieldIsWalkable b (x, y) ) = toWalkable b newPosition direction
  | otherwise = (x, y) where
    newPosition = case direction of
      North -> (x, y -1)
      East ->(x -1, y)
      South -> (x, y +1)
      West ->(x+1, y)

--This is the logic of the pink ghost, that is the ghost that tries to trap PacMan, so it aims 4 tiles in front of him. 
logicSpeedy :: Board -> Ghost -> Position -> Position -> Direction -> Direction -> Direction
logicSpeedy b g (pacX, pacY) (purX, purY) dirPac dirPur
  | (pacX, pacY) == (purX, purY) = dirPur
  | dirPac == North = closestPoint b g(toWalkable b (pacX, pacY + 4) North ) (purX, purY) dirPur
  | dirPac == East = closestPoint b g(toWalkable b(pacX + 4, pacY) East  ) (purX, purY) dirPur
  | dirPac == South = closestPoint b g(toWalkable b(pacX, pacY - 4) South ) (purX, purY) dirPur
  | otherwise = closestPoint b g(toWalkable b(pacX - 4, pacY) West ) (purX, purY) dirPur where
    
--This is the logic for Inky, the cyan ghost, it uses the position of Pacman and Blinky and doubles that vector. 
logicInky :: Board -> Ghost->  Position -> Position -> Position -> Direction -> Direction -> Direction
logicInky b g (pacX, pacY) (bliX, bliY) inkPos dirInk dirPac =
  closestPoint b g (keepInBoard b target) inkPos dirInk
  where
    (dx, dy) = case dirPac of
      North -> (0, 2)
      South -> (0, -2)
      East  -> (2, 0)
      West  -> (-2, 0)
    -- Double the vector from Blinky to the tile two steps ahead of Pac-Man.
    target = (2 * (pacX + dx) - bliX, 2 * (pacY + dy) - bliY)

-- Make sure the target position stays in board
keepInBoard :: Board -> Position -> Position
keepInBoard b (x, y) = (clamp x, clamp y)
  where
    clamp coordinate = max 0 (min (boardDimensions b - 1) coordinate)

--This sqaures the distance of two positions. 
distanceSquared :: Position -> Position -> Int
distanceSquared (x1, y1) (x2, y2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2

--This is the logic of Clyde, he chases Pacman directly, until he is within 8 cells, that he scatters to his corner. 
logicClyde :: Board -> Ghost-> Position  -> Position -> Direction -> Direction
logicClyde b g pacPos clydePos dirClyde
  | distanceSquared pacPos clydePos >= 64 = closestPoint  b g pacPos clydePos dirClyde
  | otherwise = closestPoint b  g (0, 0) clydePos dirClyde

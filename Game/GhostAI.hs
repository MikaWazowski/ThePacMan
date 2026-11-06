module Game.GhostAI where

import Game.Types
import System.Random (StdGen, randomR, randomRIO)
import System.IO.Unsafe (unsafePerformIO)

positionToDirection2:: Position -> Position -> Direction
positionToDirection2 (x, y) (x2, y2) 
  |(x > x2) = West
  |(x < x2) = East
  |(y > y2) = South
  |(y < y2) = North


fieldIsWalkable2 :: Board -> Position -> Bool
fieldIsWalkable2 b (x, y) =
  x >= 0
    && y >= 0
    && x < boardDimensions b
    && y < boardDimensions b
    && not (any wallPlace (walls b))
  where
    wallPlace w =
      let (wx, wy) = startPosition w
       in y == wy && x >= wx && x < wx + Game.Types.lengthWall w

routes :: Board -> Position -> [(Position, Direction)] -> [Position] -> Direction
--             ^ target       ^ queue                    ^ visited
routes _ _ [] _ = North
routes gameBoard (xTarget, yTarget) (((xNeighbor, yNeighbor), neighborDirection) : xs) visited = winner where
     winner
      |(xTarget, yTarget) == (xNeighbor, yNeighbor) = neighborDirection
      |otherwise = routes gameBoard (xTarget, yTarget) (xs ++ new) (newNeighbors ++ visited)
      where 
      newNeighbors = neighbours gameBoard (xNeighbor, yNeighbor) [(0,1),(1,0),(0,-1),(-1,0)] visited
      new = [ (n, neighborDirection) | n <- newNeighbors ]
    --winner = [((x, y), North)]

neighbours :: Board -> Position -> [Position] -> [Position] -> [Position]
neighbours gameBoard position [] visited = []
neighbours gameBoard (x, y) ((xcheck, ycheck) : xs) visited
  | fieldIsWalkable2 gameBoard (x + xcheck, y + ycheck) && (notElem (x + xcheck, y + ycheck) visited)= (x + xcheck, y + ycheck) : neighbours gameBoard (x, y) xs visited
  | otherwise = neighbours gameBoard (x, y) xs visited

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


oppositeDir :: Direction -> Direction
oppositeDir North = South
oppositeDir South = North
oppositeDir West = East
oppositeDir East = West

randomDirIO :: Direction -> IO Direction
randomDirIO dir = do
  let options = filter (/= oppositeDir dir) [North, East, South, West]
  i <- randomRIO (0, length options - 1)
  return (options !! i)

toWalkable :: Board -> Position -> Direction -> Position
toWalkable b (x, y) direction
  | not (fieldIsWalkable2 b (x, y) ) = toWalkable b newPosition direction
  | otherwise = (x, y) where
    newPosition = case direction of
      North -> (x, y -1)
      East ->(x -1, y)
      South -> (x, y +1)
      West ->(x+1, y)


logicPurple :: Board -> Ghost -> Position -> Position -> Direction -> Direction -> Direction
logicPurple b g (pacX, pacY) (purX, purY) dirPac dirPur
  | (pacX, pacY) == (purX, purY) = dirPur
  | dirPac == North = closestPoint b g(toWalkable b (pacX, pacY + 4) North ) (purX, purY) dirPur
  | dirPac == East = closestPoint b g(toWalkable b(pacX + 4, pacY) East  ) (purX, purY) dirPur
  | dirPac == South = closestPoint b g(toWalkable b(pacX, pacY - 4) South ) (purX, purY) dirPur
  | otherwise = closestPoint b g(toWalkable b(pacX - 4, pacY) West ) (purX, purY) dirPur where
    

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

distanceSquared :: Position -> Position -> Int
distanceSquared (x1, y1) (x2, y2) = (x1 - x2) ^ 2 + (y1 - y2) ^ 2

logicClyde :: Board -> Ghost-> Position  -> Position -> Direction -> Direction
logicClyde b g pacPos clydePos dirClyde
  | distanceSquared pacPos clydePos >= 64 = closestPoint  b g pacPos clydePos dirClyde
  | otherwise = closestPoint b  g (0, 0) clydePos dirClyde

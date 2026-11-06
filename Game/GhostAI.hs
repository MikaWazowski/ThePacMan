module Game.GhostAI where

import Game.Types
import System.Random (StdGen, randomR, randomRIO)
import System.IO.Unsafe (unsafePerformIO)

-- closestPoint :: Position -> Position -> Direction -> Direction
-- closestPoint (xPac, ypac) (xBlink, yBlink) dirBlin
--   | (xPac, ypac) == (xBlink, yBlink) = dirBlin
--   | otherwise = snd (minimum lijst)
--   where
--     -- The reason for the lack of the sqrt is because our position is a tuple of type (int, int)
--     -- For checking what is closer, the sqrt is not needed.
--     afstand = ((xPac - xBlink) ^ 2 + (ypac - yBlink) ^ 2)
--     afstandN = ((xPac - xBlink) ^ 2 + (ypac - (yBlink + 1)) ^ 2)
--     afstandE = ((xPac - (xBlink + 1)) ^ 2 + (ypac - yBlink) ^ 2)
--     afstandS = ((xPac - xBlink) ^ 2 + (ypac - (yBlink - 1)) ^ 2)
--     afstandW = ((xPac - (xBlink - 1)) ^ 2 + (ypac - yBlink) ^ 2)
--     lijst = [(afstandN, North), (afstandE, East), (afstandS, South), (afstandW, West)]
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
--             ^ doel         ^ wachtrij                 ^ visited
routes _ _ [] _ = North
routes bord (xTarget, yTarget) (((xBuren, yBuren), richtingBuur) : xs) bezochtlijst =  winnaar where
     winnaar 
      |(xTarget, yTarget) == (xBuren, yBuren)  = richtingBuur
      |otherwise = routes bord (xTarget, yTarget) (xs ++ nieuw) (nieuweburen++ bezochtlijst)
      where 
      nieuweburen = (neighbours bord (xBuren, yBuren) [(0,1),(1,0),(0,-1),(-1,0)]) bezochtlijst
      nieuw = [ (n, richtingBuur) | n <- nieuweburen ]
    --winnaart = [((x, y), North)]

neighbours :: Board -> Position -> [Position] -> [Position] -> [Position]
neighbours bord positie [] bezochtlijst = []
neighbours bord (x, y) ((xcheck, ycheck) : xs) bezochtlijst
  | fieldIsWalkable2 bord (x + xcheck, y + ycheck) && (notElem (x + xcheck, y + ycheck) bezochtlijst)= (x + xcheck, y + ycheck) : neighbours bord (x, y) xs bezochtlijst
  | otherwise = neighbours bord (x, y) xs bezochtlijst

closestPoint :: Board -> Ghost -> Position -> Position -> Direction -> Direction
closestPoint bord g doel spook huidig
  | doel == spook  = huidig
  | null start    = huidig
  | status == Afraid     =  (unsafePerformIO (randomDirIO huidig))
  | otherwise     = routes bord doel start (spook : map fst start)
  where
    start = [ (n, positionToDirection2 spook n)
            | n <- neighbours bord spook [(0,1),(1,0),(0,-1),(-1,0)] [spook] ]
    status = statusGhost g


tegenovergesteld :: Direction -> Direction 
tegenovergesteld North = South
tegenovergesteld South = North
tegenovergesteld West = East
tegenovergesteld East = West

randomDirIO :: Direction -> IO Direction
randomDirIO dir = do
  let options = filter (/= tegenovergesteld dir) [North, East, South, West]
  i <- randomRIO (0, length options - 1)
  return (options !! i)

toWalkable :: Board -> Position -> Direction -> Position
toWalkable b (x, y) richting 
  | not (fieldIsWalkable2 b (x, y) ) = toWalkable b newPosition richting
  | otherwise = (x, y) where
    newPosition = case richting of
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

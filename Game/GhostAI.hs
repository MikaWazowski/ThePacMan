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
  | status == Afraid     =  (snd (start !! unsafePerformIO (randomRIO (0, length start - 1))))
  | otherwise     = routes bord doel start (spook : map fst start)
  where
    start = [ (n, positionToDirection2 spook n)
            | n <- neighbours bord spook [(0,1),(1,0),(0,-1),(-1,0)] [spook] ]
    status = statusGhost g



toDirection :: Int -> Direction
toDirection 0 = North
toDirection 1 = East
toDirection 2 = South
toDirection _ = West

randomDirIO :: IO Direction
randomDirIO = toDirection <$> randomRIO (0, 3)

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
logicInky b g(pacX, pacY) (bliX, bliY) inkPos dirInk dirPac =
  let afstand =
        case dirPac of
          North -> (pacX + (pacX - bliX) * 2, pacY + 2 + (pacY - bliY) * 2)
          South -> (pacX + (pacX - bliX) * 2, pacY - 2 + (pacY - bliY) * 2)
          West -> (pacX - 2 + (pacX - bliX) * 2, pacY + (pacY - bliY) * 2)
          East -> (pacX + 2 + (pacX - bliX) * 2, pacY + (pacY - bliY) * 2)
   in closestPoint b g(pacX, pacY) inkPos dirInk

logicClyde :: Board -> Ghost-> Position  -> Position -> Direction -> Direction
logicClyde b g (xPac, ypac) (clyX, clyY) dirBlin 
  | (afstand (xPac, ypac) (clyX, clyY) 32) = closestPoint  b g (xPac, ypac) (clyX, clyY) dirBlin  
  -- (0,0) moet linker corner zijn
  | otherwise = closestPoint b  g (0, 0) (clyX, clyY) dirBlin

afstand :: Position -> Position -> Int -> Bool
afstand (pacX, pacY) (clyX, clyY) tileSize 
--de 1 moet eigenlijk een grid van 8 zijn, maar onze grid is te klein, dus die moeten we later groter gaan maken. 
 |abs (pacX - clyX) > 1 = True
 |otherwise =  False

-- fullTime :: Board ->  Position -> Position -> Direction -> Direction
-- fullTime posPac posGhost dirGhost = closestPoint posPac posGhost dirGhost

-- if stuck implementeren
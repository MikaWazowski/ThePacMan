module Game.Logic where

import Game.GhostAI
import Game.Types

stepPosition :: Direction -> Position -> Position
stepPosition North (x, y) = (x, y + 1)
stepPosition South (x, y) = (x, y - 1)
stepPosition East (x, y) = (x + 1, y)
stepPosition West (x, y) = (x - 1, y)

positionToDirection:: Position -> Position -> Direction
positionToDirection (x, y) (x2, y2) 
  |(x > x2) = West
  |(x < x2) = East
  |(y > y2) = South
  |(y < y2) = North

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
       in y == wy && x >= wx && x < wx + Game.Types.length w

-- routes :: Board -> Position -> [(Position, Direction)] -> [Position] -> Direction
-- --             ^ doel         ^ wachtrij                 ^ visited
-- routes _ _ [] _ = North
-- routes bord (xTarget, yTarget) (((xBuren, yBuren), richtingBuur) : xs) bezochtlijst =  winnaar where
--      winnaar 
--       |(xTarget, yTarget) == (xBuren, yBuren)  = richtingBuur
--       |otherwise = routes bord (xTarget, yTarget) (xs ++ nieuw) (nieuweburen++ bezochtlijst)
--       where 
--       nieuweburen = (neighbours bord (xBuren, yBuren) [(0,1),(1,0),(0,-1),(-1,0)]) bezochtlijst
--       nieuw = [ (n, richtingBuur) | n <- nieuweburen ]
--     --winnaart = [((x, y), North)]

-- neighbours :: Board -> Position -> [Position] -> [Position] -> [Position]
-- neighbours bord positie [] bezochtlijst = []
-- neighbours bord (x, y) ((xcheck, ycheck) : xs) bezochtlijst
--   | fieldIsWalkable bord (x + xcheck, y + ycheck) && (notElem (x + xcheck, y + ycheck) bezochtlijst)= (x + xcheck, y + ycheck) : neighbours bord (x, y) xs bezochtlijst
--   | otherwise = neighbours bord (x, y) xs bezochtlijst

movePacMan :: Board -> Direction -> PacMan -> PacMan
movePacMan b dir pac =
  if fieldIsWalkable b next
    then pac {position = next, directionGoing = dir}
    else pac
  where
    next = stepPosition dir (position pac)

moveGhost :: GameState -> Board -> Int -> Position -> Ghost -> Ghost
moveGhost state b stuckTimer posPac g
  | fieldIsWalkable b nextPos = g {positionGhost = nextPos, directionGoingGhost = dirWant, stuckTimer = 0}
  | fieldIsWalkable b nextPos = g {positionGhost = nextPos, directionGoingGhost = dirWant, stuckTimer = 0}
  | otherwise = g {stuckTimer = stuckTimer + 1}
  where
    -- if fieldIsWalkable b next
    -- then
    -- else ghost
    pac = pacMan state
    blinky = head (ghost state)
    blinkyPos = positionGhost blinky
    dirPac = directionGoing pac
    dirWant = fullTime b posPac posGhost dirGoing
    posGhost = positionGhost g
    dirGoing = directionGoingGhost g

    nextPos = case name g of
      Blinky -> stepPosition (closestPoint b posPac posGhost dirGoing) (positionGhost g)
     -- Blinky -> stepPosition (routes board posPac posGhost dirGoing) (positionGhost g)
      Pinky -> stepPosition (logicPurple b posPac posGhost dirPac dirGoing) (positionGhost g)
      Inky -> stepPosition (logicInky b posPac blinkyPos posGhost dirGoing dirPac) (positionGhost g)
      Clyde -> stepPosition (logicClyde b posPac posGhost dirGoing) (positionGhost g)

--  Clyde -> stepPosition (closestPoint posPac posGhost dirGoing) (positionGhost g)
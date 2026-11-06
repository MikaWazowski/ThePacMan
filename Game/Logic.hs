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
       in y == wy && x >= wx && x < wx + Game.Types.lengthWall w

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

moveGhost :: GameState -> Board -> Position -> Ghost -> Ghost
moveGhost state b posPac g
  | statusGhost g == Return && positionGhost g == home = g {statusGhost = Normal}
  | statusGhost g == Return && fieldIsWalkable b home =
      if fieldIsWalkable b homeNext
        then g {positionGhost = homeNext, directionGoingGhost = homeDirection}
        else g
  | statusGhost g == Return = g
  | statusGhost g == Waiting && powerActive state = g
  
  | fieldIsWalkable b nextPos = updatedGhost {positionGhost = nextPos, directionGoingGhost = direction}
  | otherwise = updatedGhost
  where
    home = ghostSpawn g
    homeDirection = closestPoint b g home (positionGhost g) (directionGoingGhost g)
    homeNext = stepPosition homeDirection (positionGhost g)
    updatedGhost = g {statusGhost = newStatus}
    newStatus
      | powerActive state = Afraid
      | otherwise = Normal
    pac = pacMan state
    blinkyPos = positionGhost (head (ghost state))
    dirPac = directionGoing pac
    posGhost = positionGhost g
    dirGoing = directionGoingGhost g

    direction = case name g of
      Blinky -> closestPoint b updatedGhost posPac posGhost dirGoing
      Pinky -> logicPurple b updatedGhost posPac posGhost dirPac dirGoing
      Inky -> logicInky b updatedGhost posPac blinkyPos posGhost dirGoing dirPac
      Clyde -> logicClyde b updatedGhost posPac posGhost dirGoing

    nextPos = stepPosition direction posGhost

powerActive :: GameState -> Bool
powerActive state = timerOngoing state < powerUntil state

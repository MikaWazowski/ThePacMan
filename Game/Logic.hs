module Game.Logic where

import Game.GhostAI
import Game.Types

stepPosition :: Direction -> Position -> Position
stepPosition North (x, y) = (x, y + 1)
stepPosition South (x, y) = (x, y - 1)
stepPosition East (x, y) = (x + 1, y)
stepPosition West (x, y) = (x - 1, y)

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

movePacMan :: Board -> Direction -> PacMan -> PacMan
movePacMan b dir pac =
  if fieldIsWalkable b next
    then pac {position = next, directionGoing = dir}
    else pac
  where
    next = stepPosition dir (position pac)

moveGhost :: GameState -> Board -> Position -> Ghost -> Ghost
moveGhost state b posPac g
  | fieldIsWalkable b nextPos =  g {positionGhost = nextPos, directionGoingGhost = dirWant}
  | otherwise = g
  where
    -- if fieldIsWalkable b next
    -- then
    -- else ghost
    pac = pacMan state
    blinky = head(ghost state)
    blinkyPos = positionGhost blinky
    dirPac =  directionGoing pac
    dirWant = fullTime posPac posGhost dirGoing
    posGhost = positionGhost g
    dirGoing = directionGoingGhost g

    nextPos =  case name g of
      Blinky -> stepPosition (closestPoint posPac posGhost dirGoing) (positionGhost g)
      Pinky -> stepPosition (logicPurple posPac posGhost dirPac dirGoing) (positionGhost g)
      Inky -> stepPosition (logicInky posPac blinkyPos posGhost dirGoing dirPac) (positionGhost g)
      Clyde -> stepPosition (logicClyde posPac posGhost dirGoing) (positionGhost g)
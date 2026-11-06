module Game.Logic where

import Game.GhostAI
import Game.InitialState
import Game.Types

--This function takes a direction and a position and returns the new position I.E taking a step. 
stepPosition :: Direction -> Position -> Position
stepPosition North (x, y) = (x, y + 1)
stepPosition South (x, y) = (x, y - 1)
stepPosition East (x, y) = (x + 1, y)
stepPosition West (x, y) = (x - 1, y)

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


--This function moves PacMan in the directon given, if possible, otherwise it does nothing. 
movePacMan :: Board -> Direction -> PacMan -> PacMan
movePacMan b dir pac =
  if fieldIsWalkable b next
    then pac {position = next, directionGoing = dir}
    else pac
  where
    next = stepPosition dir (position pac)

--This function stears the ghosts in the direction dictated by ghostAI. 
moveGhost :: GameState -> Board -> Position -> Ghost -> Ghost
moveGhost state b posPac g
  -- Ghosts after they are eaten
  | statusGhost g == Return && positionGhost g == home = g {statusGhost = Normal}
  | statusGhost g == Return && fieldIsWalkable b home =
      if fieldIsWalkable b homeNext
        then g {positionGhost = homeNext, directionGoingGhost = homeDirection}
        else g
  | statusGhost g == Return = g
  | statusGhost g == Waiting && powerActive state = g
  
  -- ghost who don't return
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
--Every ghost has a different logic, so this part accounts for that. 
    direction = case name g of
      Blinky -> closestPoint b updatedGhost posPac posGhost dirGoing
      Pinky -> logicPurple b updatedGhost posPac posGhost dirPac dirGoing
      Inky -> logicInky b updatedGhost posPac blinkyPos posGhost dirGoing dirPac
      Clyde -> logicClyde b updatedGhost posPac posGhost dirGoing

    nextPos = stepPosition direction posGhost

powerActive :: GameState -> Bool
powerActive state = timerOngoing state < powerUntil state

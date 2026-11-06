module Game.Logic where

import Game.GhostAI
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
  | fieldIsWalkable b nextPos && (stateChecker (dots state)) = g {positionGhost = nextPos, directionGoingGhost = direction, statusGhost = Afraid}
  | fieldIsWalkable b nextPos = g {positionGhost = nextPos, directionGoingGhost = direction}
  | otherwise = g 
  where
    pac = pacMan state
    blinkyPos = positionGhost (head (ghost state))
    dirPac = directionGoing pac
    posGhost = positionGhost g
    dirGoing = directionGoingGhost g
--Every ghost has a different logic, so this part accounts for that. 
    direction = case name g of
      Blinky -> closestPoint b g posPac posGhost dirGoing
      Pinky -> logicPurple b g posPac posGhost dirPac dirGoing
      Inky -> logicInky b g posPac blinkyPos posGhost dirGoing dirPac
      Clyde -> logicClyde b g posPac posGhost dirGoing

    nextPos = stepPosition direction posGhost
--This function checks the state of ghosts, by checking if a big dot has been eaten. 
stateChecker :: [Dot] -> Bool
stateChecker [] = False
stateChecker (x:xs) 
  | eaten x == True && dotType x == Big = True
  | otherwise = stateChecker xs
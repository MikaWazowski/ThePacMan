module Game.Logic where
import Game.Types
import Game.GhostAI


stepPosition :: Direction -> Position -> Position
stepPosition North (x,y) = (x,y+1)
stepPosition South (x,y) = (x, y-1)
stepPosition East (x,y) = (x+1, y)
stepPosition West (x,y) = (x-1,y)


fieldIsWalkable :: Board -> Position -> Bool
fieldIsWalkable b (x, y) =
  x >= 0 && y >= 0 &&
  x < boardDimensions b && y < boardDimensions b &&
  not (any wallPlace (walls b))
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


  
moveGhost :: Board -> Direction -> Position -> Position -> Ghost -> Ghost
moveGhost b dirGoing posPac posGhost ghost =
  if fieldIsWalkable b next
  then ghost {positionGhost = next, directionGoingGhost = dirWant}
  else ghost

  where
    next = stepPosition dirWant (positionGhost ghost)
    dirWant  = fullTime posPac posGhost dirGoing
  
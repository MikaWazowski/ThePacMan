module Game.GhostAI where

import Game.Types

closestPoint :: Position -> Position -> Direction -> Direction
closestPoint (xPac, ypac) (xBlink, yBlink) dirBlin
  | (xPac, ypac) == (xBlink, yBlink) = dirBlin
  | otherwise = snd (minimum lijst)
  where
    -- The reason for the lack of the sqrt is because our position is a tuple of type (int, int)
    -- For checking what is closer, the sqrt is not needed.
    afstand = ((xPac - xBlink) ^ 2 + (ypac - yBlink) ^ 2)
    afstandN = ((xPac - xBlink) ^ 2 + (ypac - (yBlink + 1)) ^ 2)
    afstandE = ((xPac - (xBlink + 1)) ^ 2 + (ypac - yBlink) ^ 2)
    afstandS = ((xPac - xBlink) ^ 2 + (ypac - (yBlink - 1)) ^ 2)
    afstandW = ((xPac - (xBlink - 1)) ^ 2 + (ypac - yBlink) ^ 2)
    lijst = [(afstandN, North), (afstandE, East), (afstandS, South), (afstandW, West)]

logicPurple :: Position -> Position -> Direction -> Direction -> Direction
logicPurple (pacX, pacY) (purX, purY) dirPac dirPur
  | (pacX, pacY) == (purX, purY) = dirPur
  | dirPac == North = closestPoint (pacX, pacY + 4) (purX, purY) dirPur
  | dirPac == East = closestPoint (pacX + 4, pacY) (purX, purY) dirPur
  | dirPac == South = closestPoint (pacX, pacY - 4) (purX, purY) dirPur
  | otherwise = closestPoint (pacX - 4, pacY) (purX, purY) dirPur

logicInky :: Position -> Position -> Position -> Direction -> Direction -> Direction
logicInky (pacX, pacY) (bliX, bliY) inkPos dirInk dirPac =
  let afstand =
        case dirPac of
          North -> (pacX + (pacX - bliX) * 2, pacY + 2 + (pacY - bliY) * 2)
          South -> (pacX + (pacX - bliX) * 2, pacY - 2 + (pacY - bliY) * 2)
          West -> (pacX - 2 + (pacX - bliX) * 2, pacY + (pacY - bliY) * 2)
          East -> (pacX + 2 + (pacX - bliX) * 2, pacY + (pacY - bliY) * 2)
   in closestPoint (pacX, pacY) inkPos dirInk

logicClyde :: Position  -> Position -> Direction -> Direction
logicClyde (xPac, ypac) (clyX, clyY) dirBlin
  | (afstand (xPac, ypac) (clyX, clyY)) > 64 = closestPoint (xPac, ypac) (clyX, clyY) dirBlin
  -- (0,0) moet linker corner zijn
  | otherwise = closestPoint (0, 0) (clyX, clyY) dirBlin

afstand :: Position -> Position -> Int
afstand (pacX, pacY) (clyX, clyY) = (pacX - clyX) ^ 2 + (pacY - clyY) ^ 2

fullTime :: Position -> Position -> Direction -> Direction
fullTime posPac posGhost dirGhost = closestPoint posPac posGhost dirGhost

-- if stuck implementeren
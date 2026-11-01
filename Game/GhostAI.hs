module Game.GhostAI where
import Game.Types

closestPoint ::  Position -> Position -> Direction -> Direction
closestPoint (xPac, ypac) (xBlink, yBlink) dirBlin | (xPac, ypac) == (xBlink, yBlink) = dirBlin
                                    | afstandN < afstand = North
                                    | afstandE < afstand = East
                                    | afstandS < afstand = South
                                    | otherwise = West where
                                        afstand =  ((xPac-xBlink)^2 + (ypac-yBlink)^2)
                                        afstandN =  ((xPac-xBlink)^2 + (ypac-yBlink+1)^2)
                                        afstandE =  ((xPac-xBlink+1)^2 + (ypac-yBlink)^2)
                                        afstandS =  ((xPac-xBlink)^2 + (ypac-yBlink-1)^2)
                                       
logicPurple :: Position -> Position -> Direction  -> Direction -> Direction 
logicPurple (pacX, pacY) (purX, purY) dirPac dirPur | (pacX, pacY) == (purX, purY) = dirPur
                                                    | dirPac == North= closestPoint(pacX, pacY+4)  (purX, purY) dirPur
                                                    | dirPac == East = closestPoint(pacX+4, pacY)  (purX, purY) dirPur
                                                    | dirPac == South = closestPoint(pacX, pacY-4)  (purX, purY) dirPur
                                                    | otherwise =closestPoint(pacX-4, pacY)  (purX, purY) dirPur
                                        




logicCross:: [Position] -> Position -> Direction -> Position-> Direction
logicCross [] _ richting _ = richting
logicCross (x:xs) positie richting pacPos | x == positie = closestPoint pacPos positie richting
                                          | otherwise = logicCross xs positie richting pacPos

fullTime ::  Position -> Position -> Direction -> Direction
fullTime posPac posGhost dirGhost = closestPoint posPac posGhost dirGhost

  
  --if stuck implementeren
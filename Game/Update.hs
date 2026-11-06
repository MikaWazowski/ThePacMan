module Game.Update where

import Game.Logic
import Game.Types


step :: Float -> GameState -> GameState
step time state
  | statusGame state /= Ongoing = state -- The game is not running so no timer has to be run
  | otherwise = (updatePacMan time state False [] []) { timerOngoing = timerOngoing state + time }

--This function updates the gamestate considering new walls, moved pacman, moved Ghosts and new dots. 
updatePacMan :: Float -> GameState -> Bool ->[Wall] -> [Dot] -> GameState
updatePacMan time state wallChange muren dotsChange 
--Check if the map needs changing I.E are there new walls/dots. 
  | wallChange = changeMap state muren dotsChange
  | otherwise = moveTicks ticks state
  where
    oldTime = timerOngoing state
    --The speed ghosts and PacMan moves. 
    movementSpeed = 3
    --Check if the ghosts are afraid. It does this by checking if a large dot has been eaten. 
    ghostAfraid = stateChecker (dots state)
    -- currently step gets called 30 times a second, but packman only moves 5 times per second
    ticks = floor ((oldTime + time) * movementSpeed) - floor (oldTime * movementSpeed)
    
    moveTicks n currentState
      | n <= 0 || statusGame currentState /= Ongoing = currentState
      | otherwise = moveTicks (n - 1) (moveOne currentState)
    --Guards for different outcomes if PacMan hits one of the ghosts. 
    moveOne currentState
    --If the ghost collides with PacMan and the ghost is afraid. The score will increase by 100 and the ghost will be eaten. 
      | collided && ghostAfraid = currentState { pacMan = movedPac, ghost = ghostCollided (position movedPac) movedGhosts, score = oldscore + 100 }
    --If the ghost collides while is it nog afraid, PacMan dies and the game is lost. 
      | collided = currentState { pacMan = movedPac, ghost = movedGhosts, statusGame = Lost }
    --Otherwise it checks if the dots are collected. 
      | otherwise = collectDots (currentState { pacMan = movedPac, ghost = movedGhosts })
      where
        -- Setting local "variables" from the current state of the game
        gameBoard = board currentState
        pac = pacMan currentState
        wantedDirection = directionWant pac
        oldscore = score currentState
        -- See if there is no wall that makes it so that pacman cannot move into its wanted direction
        moveWantedDir = fieldIsWalkable gameBoard (stepPosition wantedDirection (position pac))

        -- Direction pacman will be moving
        direction
          | moveWantedDir   = wantedDirection
          | otherwise = directionGoing pac

        -- Calculates next position of pacman
        movedPac = movePacMan gameBoard direction pac

        --List of ghost all moved in the direction dictated by the algorithm. 
        movedGhosts = map (moveGhost currentState gameBoard (position movedPac)) (ghost currentState)
        --Quick Lambda function to check if the ghost hits PacMan. 
        collided = any (\x -> positionGhost x == position movedPac) movedGhosts

--This function returns a list of ghosts, but with the change that if a ghost is eaten, its status is return. 
ghostCollided :: (Position) -> [Ghost] ->  [Ghost]
ghostCollided positie [] = []
ghostCollided positie (x:xs) 
  | positie == (positionGhost x) = x {statusGhost = Return} : ghostCollided positie xs
  | otherwise = x : ghostCollided positie xs

--Function that counts the score of the game with eaten dots. 
collectDots :: GameState -> GameState
-- Disadvantage of this is that you calculate this for every dot, every tick but i don't know how to fix it otherwise
collectDots state = state {dots = map eatDot (dots state), score = score state + sum [dotScore dot | dot <- dots state, posPacDot dot]}
  where
    -- Is pacman currently in the position of an uneated dot
    posPacDot dot = not (eaten dot) && dotPosition dot == position (pacMan state)

    eatDot dot
      | posPacDot dot = dot { eaten = True }
      | otherwise = dot

    dotScore dot = case dotType dot of
      Small -> 10
      Big -> 50
--This function changes the map with new dots and walls. 
changeMap:: GameState -> [Wall] -> [Dot]-> GameState
changeMap state wallsNieuw newDots = state {board = nieuwboard, dots = newDots} where
  nieuwboard = (board state) {walls = wallsNieuw}
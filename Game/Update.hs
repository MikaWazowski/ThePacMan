module Game.Update where

import Game.Logic
import Game.Types

step :: Float -> GameState -> GameState
step time state
  | statusGame state /= Ongoing = state -- The game is not running so no timer has to be run
  | otherwise = (updatePacMan time state) { timerOngoing = timerOngoing state + time }

updatePacMan :: Float -> GameState -> GameState
updatePacMan time state = moveTicks ticks state
  where
    oldTime = timerOngoing state
    movementSpeed = 5

    -- currently step gets called 30 times a second, but packman only moves 5 times per second
    ticks = floor ((oldTime + time) * movementSpeed) - floor (oldTime * movementSpeed)

    moveTicks n currentState
      | n <= 0 || statusGame currentState /= Ongoing = currentState
      | otherwise = moveTicks (n - 1) (moveOne currentState)

    moveOne currentState
      | collided = currentState { pacMan = movedPac, ghost = movedGhosts, statusGame = Lost }
      | otherwise = collectDots (currentState { pacMan = movedPac, ghost = movedGhosts })
      where
        -- Setting local "variables" from the current state of the game
        gameBoard = board currentState
        pac = pacMan currentState
        wantedDirection = directionWant pac

        -- See if there is no wall that makes it so that pacman cannot move into its wanted direction
        moveWantedDir = fieldIsWalkable gameBoard (stepPosition wantedDirection (position pac))

        -- Direction pacman will be moving
        direction
          | moveWantedDir   = wantedDirection
          | otherwise = directionGoing pac

        -- Calculates next position of pacman
        movedPac = movePacMan gameBoard direction pac

        
        movedGhosts = map (moveGhost currentState gameBoard (position movedPac)) (ghost currentState)

        collided = any (\x -> positionGhost x == position movedPac) movedGhosts

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

module Game.Update where

import Game.Logic
import Game.Types

step :: Float -> GameState -> GameState
step time state
  | statusGame state /= Ongoing = state -- The game is not running so no timer has to be run
  | otherwise = updateScore time (updatePacMan time state)

updatePacMan :: Float -> GameState -> GameState
updatePacMan time state = moveTicks ticks state
  where
    oldTime = timerOngoing state
    movementSpeed = 5

    -- currently step gets called 30 times a second, but packman only moves 5 times per second
    ticks = floor ((oldTime + time) * movementSpeed) - floor (oldTime * movementSpeed)

    moveTicks n currentState
      | n <= 0 = currentState
      | otherwise = moveTicks (n - 1) (moveOne currentState)

    moveOne currentState = currentState {pacMan = movePacMan gameBoard direction pac, ghost = ghosts}
      where
        gameBoard = board currentState
        pac = pacMan currentState
        wanted = directionWant pac
        ghostvarlijst = ghost currentState
        pacPos = position pac
        ghosts = map (moveGhost currentState gameBoard pacPos) ghostvarlijst
       -- ghostPos = positionGhost ghostMoved
     

        --moveGhost :: Board -> Position -> Ghost -> Ghost
       -- directionGhost = directionGoingGhost ghostMoved
        direction =
          if fieldIsWalkable gameBoard (stepPosition wanted (position pac))
            then wanted
            else directionGoing pac



-- updateGhost :: Float -> GameState -> GameState
-- updateGhost time state = moveTicks ticks state
--   where
--     oldTime = timerOngoing state
--     movementSpeed = 5

--     -- currently step gets called 30 times a second, but packman only moves 5 times per second
--     ticks = floor ((oldTime + time) * movementSpeed) - floor (oldTime * movementSpeed)

--     moveTicks n currentState
--       | n <= 0 = currentState
--       | otherwise = moveTicks (n - 1) (moveOne currentState)

--     moveOne currentState = currentState {ghost = [moveGhost gameBoard direction pacPos ghostPos ghostvar]}
--       where
--         gameBoard = board currentState
--         pac = pacMan currentState
--         ghostvarlijst = ghost currentState
--         ghostvar = head ghostvarlijst
--         pacPos = position pac
--         ghostPos = positionGhost ghostvar
--         wanted = directionWant pac
--         direction = directionGoingGhost ghostvar

updateScore :: Float -> GameState -> GameState
updateScore time state =
  state {timerOngoing = newTime, score = score state + addedScore}
  where
    oldTime = timerOngoing state
    newTime = oldTime + time
    addedScore = floor newTime - floor oldTime
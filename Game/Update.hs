module Game.Update where

import Game.Logic
import Game.InitialState
import Game.Types

step :: Float -> GameState -> GameState
step time state
  | statusGame state /= Ongoing = state -- The game is not running so no timer has to be run
  | otherwise = (updatePacMan time state False [] []) { timerOngoing = timerOngoing state + time }

updatePacMan :: Float -> GameState -> Bool ->[Wall] -> [Dot] -> GameState
updatePacMan time state wallChange muren dotsChange |
  wallChange = changeMap state muren dotsChange
  | otherwise = moveTicks ticks state
  where
    oldTime = timerOngoing state
    movementSpeed = 5
    -- currently step gets called 30 times a second, but packman only moves 5 times per second
    ticks = floor ((oldTime + time) * movementSpeed) - floor (oldTime * movementSpeed)
    firstTick = floor (oldTime * movementSpeed) + 1
    
    moveTicks n currentState
      | n <= 0 || statusGame currentState /= Ongoing = currentState
      | otherwise = moveTicks (n - 1) (moveOne (currentState {timerOngoing = tickTime}))
      where
        tickTime = fromIntegral (firstTick + ticks - n) / movementSpeed

    moveOne currentState
      | any activeGhost collidingGhosts = nextState {statusGame = Lost}
      | all eaten (dots nextState) = scoredState {statusGame = Won}
      | otherwise = scoredState
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
        nextState = collectDots (currentState {pacMan = movedPac, ghost = movedGhosts})
        
        collidingGhosts = filter ((== position movedPac) . positionGhost) movedGhosts
        activeGhost g = statusGhost g /= Return && not (powerActive nextState)
        
        edibleGhosts = filter (\g -> statusGhost g /= Return && powerActive nextState) collidingGhosts

        -- Get 100 points for each ghost you eat
        ghostScore = 100 * length edibleGhosts
        scoredState = nextState {ghost = map eatCollidingGhost movedGhosts, score = score nextState + ghostScore}
        eatCollidingGhost g
          | statusGhost g /= Return && positionGhost g == position movedPac && powerActive nextState = g {statusGhost = Return}
          | otherwise = g


collectDots :: GameState -> GameState
-- Disadvantage of this is that you calculate this for every dot, every tick but i don't know how to fix it otherwise
collectDots state = state {dots = map eatDot (dots state), score = score state + sum [dotScore dot | dot <- collectedDots], powerUntil = newPowerUntil}
  where
    -- Is pacman currently in the position of an uneated dot
    posPacDot dot = not (eaten dot) && dotPosition dot == position (pacMan state)
    collectedDots = filter posPacDot (dots state)


    newPowerUntil
      | any ((== Big) . dotType) collectedDots = timerOngoing state + 4
      | otherwise = powerUntil state

    eatDot dot
      | posPacDot dot = dot { eaten = True }
      | otherwise = dot

    dotScore dot = case dotType dot of
      Small -> 10
      Big -> 50

changeMap:: GameState -> [Wall] -> [Dot]-> GameState
changeMap state wallsNieuw newDots = addSmallDots (state {board = nieuwboard, dots = newDots}) where
  nieuwboard = (board state) {walls = wallsNieuw}

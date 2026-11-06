module Game.Model where

import Game.Types
import Game.GhostAI

initialState :: GameState
initialState = addSmallDots GameState
  { board = startingBoard
  , pacMan = PacMan middle East East
  , ghost = [ Ghost Blinky (0, 0) Normal East (0, 0)
            , Ghost Pinky (lastCell, 0) Normal North (lastCell, 0)
            , Ghost Clyde (0, lastCell) Normal North (0, lastCell)
            , Ghost Inky (lastCell, lastCell) Normal North (lastCell, lastCell)
            ]
  , dots = [ Dot False Big (2, 14)
           , Dot False Big (13, 14)
           , Dot False Big (2, 1)
           , Dot False Big (13, 1)
           ]
  , score = 0
  , statusGame = Ongoing
  , timerOngoing = 0
  , powerUntil = 0
  }
  where
    startingBoard = Board
      [ Wall 3 (2, 13), Wall 2 (7, 13), Wall 3 (11, 13)
      , Wall 1 (4, 12), Wall 1 (11, 12)
      , Wall 2 (1, 11), Wall 1 (4, 11), Wall 4 (6, 11), Wall 1 (11, 11), Wall 2 (13, 11)
      , Wall 1 (4, 10), Wall 1 (11, 10)
      , Wall 3 (2, 9), Wall 1 (6, 9), Wall 1 (9, 9), Wall 3 (11, 9)
      , Wall 1 (6, 8), Wall 1 (9, 8)
      , Wall 1 (6, 7), Wall 1 (9, 7)
      , Wall 3 (2, 6), Wall 1 (6, 6), Wall 1 (9, 6), Wall 3 (11, 6)
      , Wall 1 (4, 5), Wall 1 (11, 5)
      , Wall 2 (1, 4), Wall 1 (4, 4), Wall 4 (6, 4), Wall 1 (11, 4), Wall 2 (13, 4)
      , Wall 1 (4, 3), Wall 1 (11, 3)
      , Wall 3 (2, 2), Wall 2 (7, 2), Wall 3 (11, 2)
      ] 16
    size = boardDimensions startingBoard
    lastCell = size - 1
    middle = (size `div` 2, size `div` 2)

-- Fill every free cell with one small pellet, preserving the level's power pellets.
addSmallDots :: GameState -> GameState
addSmallDots state = state {dots = bigDots ++ smallDots}
  where
    gameBoard = board state
    size = boardDimensions gameBoard
    bigDots = filter ((== Big) . dotType) (dots state)
    occupied cell = cell == position (pacMan state) || any ((== cell) . ghostSpawn) (ghost state) || any ((== cell) . dotPosition) bigDots || wallAt gameBoard cell
    -- Every position that isn't taken is filled with a small dot
    smallDots =
      [ Dot False Small cell | x <- [0 .. size - 1], y <- [0 .. size - 1], let cell = (x, y), not (occupied cell)]

-- Gives a bool if there is a wall on that position. Because of our logic that a wall can stretch multiple cells we cannot just check the position itself.
wallAt :: Board -> Position -> Bool
wallAt gameBoard (x, y) = any alreadyWall (walls gameBoard)
  where
    alreadyWall wall =
      let (wallX, wallY) = startPosition wall
       in y == wallY && x >= wallX && x < wallX + lengthWall wall

--This function takes a direction and a position and returns the new position I.E taking a step. 
stepPosition :: Direction -> Position -> Position
stepPosition North (x, y) = (x, y + 1)
stepPosition South (x, y) = (x, y - 1)
stepPosition East (x, y) = (x + 1, y)
stepPosition West (x, y) = (x - 1, y)

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
      Pinky -> logicSpeedy b updatedGhost posPac posGhost dirPac dirGoing
      Inky -> logicInky b updatedGhost posPac blinkyPos posGhost dirGoing dirPac
      Clyde -> logicClyde b updatedGhost posPac posGhost dirGoing

    nextPos = stepPosition direction posGhost

powerActive :: GameState -> Bool
powerActive state = timerOngoing state < powerUntil state

step :: Float -> GameState -> GameState
step time state
  | statusGame state /= Ongoing = state -- The game is not running so no timer has to be run
  | otherwise = (updatePacMan time state False [] []) { timerOngoing = timerOngoing state + time }

--This function updates the gamestate considering new walls, moved pacman, moved Ghosts and new dots. 
updatePacMan :: Float -> GameState -> Bool ->[Wall] -> [Dot] -> GameState
updatePacMan time state wallChange newWalls dotsChange
--Check if the map needs changing I.E are there new walls/dots. 
  | wallChange = changeMap state newWalls dotsChange
  | otherwise = moveTicks ticks state
  where
    oldTime = timerOngoing state
    -- Speed of the ghosts and pacman
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

        --List of ghost all moved in the direction dictated by the algorithm. 
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

--Function that counts the score of the game with eaten dots. 
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
--This function changes the map with new dots and walls. 
changeMap:: GameState -> [Wall] -> [Dot]-> GameState
changeMap state newWalls newDots = addSmallDots (state {board = newBoard, dots = newDots}) where
  newBoard = (board state) {walls = newWalls}

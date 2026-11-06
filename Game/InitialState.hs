module Game.InitialState where

import Game.Types

initialState :: GameState
initialState = addSmallDots GameState
  { board = startingBoard
  , pacMan = PacMan middle East East
  , ghost = [ Ghost Blinky (0, 0) Normal East (0, 0)
            , Ghost Pinky (lastCell, 0) Normal North (lastCell, 0)
            , Ghost Clyde (0, lastCell) Normal North (0, lastCell)
            , Ghost Inky (lastCell, lastCell) Normal North (lastCell, lastCell)
            ]
  , dots = [ Dot False Big (8, 8)
           , Dot False Big (0, 0)
           , Dot False Big (8, 0)
           , Dot False Big (0, 8)
           ]
  , score = 0
  , lives = 3
  , statusGame = Ongoing
  , timerOngoing = 0
  , powerUntil = 0
  }
  where
    startingBoard = Board [] 16
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

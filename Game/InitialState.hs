module Game.InitialState where

import Game.Types

initialState :: GameState
initialState = GameState
  { board = startingBoard
  , pacMan = PacMan middle East East
  , ghost = [ Ghost Blinky (0, 0) Normal East 0
            , Ghost Pinky (lastCell, 0) Normal North 0
            , Ghost Clyde (0, lastCell) Normal North 0
            , Ghost Inky (lastCell, lastCell) Normal North 0
            ]
  , dots = [ Dot False Small (2, 5)
           , Dot False Small (1, 1)
           , Dot False Small (3, 3)
           , Dot False Small (3, 6)
           , Dot False Small (5, 8)
           , Dot False Big (8, 8)
           , Dot False Big (0, 0)
           , Dot False Big (8, 0)
           , Dot False Big (0, 8)
           ]
  , score = 0
  , lives = 3
  , statusGame = Ongoing
  , timerOngoing = 0
  }
  where
    startingBoard = Board [] 16
    size = boardDimensions startingBoard
    lastCell = size - 1
    middle = (size `div` 2, size `div` 2)

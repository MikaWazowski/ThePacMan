module Game.InitialState where

import Game.Types

initialState :: GameState
initialState = GameState
  { board = Board [Wall 4 (3, 3), Wall 3 (2, 7)] 10
  , pacMan = PacMan (1, 1) East East
  , ghost = [ Ghost Blinky (5, 5) Normal East 0
            , Ghost Pinky (2, 2) Normal North 0
            , Ghost Clyde (8, 8) Normal North 0
            , Ghost Inky (9, 3) Normal North 0
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

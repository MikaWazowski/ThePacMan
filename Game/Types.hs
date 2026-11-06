module Game.Types where

type Position = (Int, Int)
data Direction = North | East | West | South deriving (Ord)
instance Eq Direction where
    North == North = True
    East == East = True
    West == West = True
    South == South = True
    _ == _ = False


-- The length of a wall is always from left to right
data Wall = Wall {
    lengthWall :: Int,
    startPosition :: Position
} deriving (Show)

data Board = Board {
    walls :: [Wall],
    boardDimensions :: Int
}

data PacMan = PacMan {
    position :: Position,
    directionGoing :: Direction,
    directionWant :: Direction
}

data GhostType = Blinky | Pinky | Inky | Clyde
    deriving (Eq)
data GhostStatus = Afraid | Normal | Return | Waiting
    deriving (Eq)

data Ghost = Ghost {
    name :: GhostType,
    positionGhost :: Position,
    statusGhost :: GhostStatus,
    directionGoingGhost :: Direction,
    ghostSpawn :: Position
}

data DotType = Small | Big
    deriving (Eq)

data Dot = Dot {
    eaten :: Bool,
    dotType :: DotType,
    dotPosition :: Position
}

data GameStatus = Ongoing | Paused | NStarted | Lost | Won | Home |Custom| Editing
    deriving (Eq)

data GameState = GameState {
    board :: Board,
    pacMan :: PacMan,
    ghost :: [Ghost],
    dots :: [Dot],
    score :: Int,
    lives :: Int,
    statusGame :: GameStatus,
    timerOngoing :: Float,
    powerUntil :: Float
}


module Main where
main = putStrLn "O| | \n-+-+-\n |X| \n-+-+-\n | | \n"

type Position = (Int, Int)
data Direction = North | East | West | South

data Wall = Wall {
    length :: Int,
    startPosition :: Position
}

data Board = Board {
    walls :: [Wall],
    boardDimensions :: Int
}

data PacMan = PacMan {
    position :: Position,
    directionGoing :: Direction,
    directionWant :: Maybe Direction
}

data GhostType = Blinky | Pinky | Inky | Clyde
data GhostStatus = Afraid Float | Normal | Return | Waiting

data Ghost = Ghost {
    name :: GhostType,
    positionGhost :: Position,
    statusGhost :: GhostStatus,
    directionGoingGhost :: Direction
}

data DotType = Small | Big
data Dot = Dot {
    eaten :: Bool,
    dotType :: DotType,
    dotPosition :: Position
}

data GameStatus = Ongoing | Paused | NStarted | Lost | Won

data GameState = GameState {
    board :: Board,
    pacMan :: PacMan,
    ghost :: [Ghost],
    dots :: [Dot],
    score :: Int,
    lives :: Int,
    statusGame :: GameStatus,
    timerOngoing :: Float
}
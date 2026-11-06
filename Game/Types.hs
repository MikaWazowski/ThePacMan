module Game.Types where

--Position type for every location in the game. 
type Position = (Int, Int)

--Data type that defines every direction possible. 
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

--The board of the game, containing a list of walls and the dimensions of the board. 
data Board = Board {
    walls :: [Wall],
    boardDimensions :: Int
}

--The Data type for the PacMan, containing the position, direction it is going and the direction it wants to go I.E the input from the user. 
data PacMan = PacMan {
    position :: Position,
    directionGoing :: Direction,
    directionWant :: Direction
}

--Datatype for the Ghost, these are all the different types of ghost. 
data GhostType = Blinky | Pinky | Inky | Clyde
    deriving (Eq)

--Data type for the status of the ghost, meaning does the ghost run away, does it chase etc. 
data GhostStatus = Afraid | Normal | Return | Waiting
    deriving (Eq)

--Data type for the ghost, containing its type, position, status and direction
data Ghost = Ghost {
    name :: GhostType,
    positionGhost :: Position,
    statusGhost :: GhostStatus,
    directionGoingGhost :: Direction,
    ghostSpawn :: Position
}

--The data type for the dots PacMan eats, they can either be big or small. 
data DotType = Small | Big
    deriving (Eq)

--Data type for the dots, containing its the status of the dot, type and the position. 
data Dot = Dot {
    eaten :: Bool,
    dotType :: DotType,
    dotPosition :: Position
}

--The status of the game, it can either be ongoing, paused, Nstarded, lost, won and there is the option for the home and custom screen. 
data GameStatus = Ongoing | Paused | NStarted | Lost | Won | Custom | Editing
    deriving (Eq)

--The datatype for the gamestate, containing everything needed to create the game. 
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

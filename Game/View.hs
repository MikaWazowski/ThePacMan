module Game.View where

import Game.Types
import Graphics.Gloss
import Game.Model

-- Needs to be float since the gloss functions expect float numbers, not integers
-- Everything other drawing will be based of tileSize, meaning that when we change the tileSize, everything scales correctly with it.
tileSize :: Float
tileSize = 32

render :: GameState -> Picture

-- the render function puts all different pictures together to create the gameboard. 
render state = pictures (grid ++ wallPictures ++ pelletPicture ++ ghostPictures ++ [pacPicture, scorePicture, statusPicture])
  where
    gameBoard = board state
    size = boardDimensions gameBoard

    -- since tileSize is a float we need to convert the int of boardDimensions
    boardWidth = fromIntegral size * tileSize

    -- placing the text that shows the score in the correct part of the screen
    scorePicture = translate (-boardWidth / 2) (-boardWidth / 2 - 40) (scale 0.2 0.2 (drawScore state))

    -- Show the full screen status updates depending in what game state you are.
    statusPicture = case statusGame state of
      Lost   -> drawStatusOverlay boardWidth "GAME OVER" "Press SPACE to restart" 0.3
      Won    -> drawStatusOverlay boardWidth "YOU WIN" "Press SPACE to restart" 0.2
      Paused -> drawStatusOverlay boardWidth "PAUSED" "Press SPACE to resume" 0.2
      Custom -> drawStatusOverlay boardWidth "CUSTOM LEVEL" "SPACE: continue - C: new level" 0.3
      _      -> blank

    -- We use a 0 based grid since that is easier for future calculations.
    -- Draw the grid with all tiles. In the tiles the objects are already represented (except the walls)
    grid = [drawTile size (x, y) (color blue (rectangleWire tileSize tileSize)) | x <- [0 .. size - 1], y <- [0 .. size - 1]]

    -- For all walls that are defined, draw the wall.
    wallPictures = concatMap (drawWall size) (walls gameBoard)
    
    -- Draw pacman image. This image changes depending on the direct PacMan is going.
    pacPicture = drawTile size (position (pacMan state)) (rotate angle (pictures [body, mouth, eye]))
      where
        -- the radius of the body of pacman
        radius = tileSize * 0.36

        -- picture object of the body of pacman
        body = color yellow (circleSolid radius)
        
        -- Each mouth stage lasts about ten updates at 30 FPS.        
        mouthFrame = floor (timerOngoing state * 3) `mod` 2

        -- There are 2 different stages of an open mouth, making it together with a closed mouth a 3 step animation
        mouthHeight 
          | mouthFrame == 0 = radius / 2
          | otherwise = radius / 5

        mouth = color black (polygon [(0, 0), (radius, mouthHeight), (radius, -mouthHeight)])

        eye = translate (-radius * 0.15) eyeY (color black (circleSolid (radius * 0.2)))

        -- Since pacman's eye is always at the top side of its body no matter what direction he is facing, we move on in the west state to compensate for the rotation (only in west the eye would be in the bottom half of pacman's body)
        eyeY = case directionGoing (pacMan state) of
          West -> -radius * 0.45
          _ -> radius * 0.45

        -- Change the mouth and eye accordingly to the dir pacman is facing.
        angle = case directionGoing (pacMan state) of
          East -> 0
          South -> 90
          West -> 180
          North -> 270

    -- Draw a dot for every dot that is not eaten yet.
    pelletPicture = [drawTile size (dotPosition dot) (drawPellet (dotType dot)) | dot <- dots state, not (eaten dot)]

    -- Draw a ghost for every ghost that is not returning to its position after being eaten.
    ghostPictures = [drawTile size (positionGhost g) (drawGhost state g) | g <- ghost state, statusGhost g /= Return]

-- Create a default overlay that sets a black transparent rectangle over the board an creates a title and message font so messages displayed above can be changed.
drawStatusOverlay :: Float -> String -> String -> Float -> Picture
drawStatusOverlay boardWidth title message middleTitle = pictures
  [ 
    --Background
    color (makeColor 0 0 0 0.7) (rectangleSolid boardWidth boardWidth)
    -- Title
  , translate (-boardWidth * middleTitle) 0 (scale (boardWidth / 1400) (boardWidth / 1400) (color red (text title)))
    -- Message what actions you can take
  , translate (-boardWidth * 0.25) (-tileSize) (scale (boardWidth / 3400) (boardWidth / 3400) (color white (text message)))
  ]

-- drawing a wall needs the extra logic of a StartX and because we define walls in horizontal lengths.
drawWall :: Int -> Wall -> [Picture]
drawWall size wall = [drawTile size (x, wallY) (color blue (rectangleSolid tileSize tileSize)) | x <- [wallStartX .. wallEndX]]
  where
    (wallStartX, wallY) = startPosition wall
    wallEndX = wallStartX + lengthWall wall - 1

-- draws the tile with all objects in it (the picture you give with a drawTile)
drawTile :: Int -> Position -> Picture -> Picture
drawTile size (x, y) picture = translate screenX screenY picture
  where
    -- size * tileSize / 2 centers the board to be in the middle of the window
    -- tileSize / 2 makes sure that the starting point of drawing the rectangle is also in the middle and not on the edge of the first tile
    screenX = fromIntegral x * tileSize - fromIntegral size * tileSize / 2 + tileSize / 2
    screenY = fromIntegral y * tileSize - fromIntegral size * tileSize / 2 + tileSize / 2

-- Draws the current score in text format under the board.
drawScore :: GameState -> Picture
drawScore state = color white (text (show scoreShow))
  where
    scoreShow = score state

-- draws either a small green powerpellet or a large red power pellet
drawPellet :: DotType -> Picture
drawPellet Small = color green (circleSolid (tileSize * 0.2))
drawPellet Big = color red (circleSolid (tileSize * 0.5))

-- Drawing the ghost
drawGhost :: GameState -> Ghost -> Picture
drawGhost state g  = pictures (colorG body : [eye x | x <- [radius * 0.2, radius * 0.8]])
  where
    radius = tileSize * 0.3
    body = pictures [
                      -- head
                      translate 0 (tileSize * 0.1) (circleSolid radius),

                      -- rectangle for main body
                      translate 0 (-tileSize * 0.1) (rectangleSolid (2 * radius) (tileSize * 0.3)),

                      -- 3 small circles that form the lower part of the body
                      pictures [translate x (-tileSize * 0.3) (circleSolid (radius / 3)) | x <- [-2 * radius / 3, 0, 2 * radius / 3]]
                    ]


    -- Creating both the white eyeball and the pupil in it
    eye x = translate eyePositionX (tileSize * 0.045 + eyeY) (pictures [color white (circleSolid (radius * 0.3)), translate pupilX pupilY (color blue (circleSolid (radius * 0.12)))])
      where
        -- The white eyeball have to change of place when the ghost is going right for the same direction the eye of pacman had to change.
        eyePositionX = case directionGoingGhost g of
          West -> -x + eyeX
          _ -> x + eyeX

    placement = radius * 0.2

    -- Makes sure the pupil of the ghosts eye looks in the direction where the ghost is heading
    (eyeX, eyeY, pupilX, pupilY) = case directionGoingGhost g of
      West  -> (-placement, 0, -placement, 0)
      East  -> (placement, 0, placement, 0)
      North -> (0, 0, 0, placement)
      South -> (0, 0, 0, -placement)

    -- deciding what color a ghost has to be, including the alternating color during the afraid state.
    colorPicker color (x, y) status
         |status == Return = black
         | not (powerActive state) = color
         | (x`mod`2 == 0 && y `mod` 2 == 0 ) || (x `mod` 2 /= 0 && y`mod`2 /= 0) = blue 
         | otherwise = white

    -- Give the correct color to the ghost with as key their name in their normal state
    colorG = case name g of
       Blinky -> color (colorPicker red (positionGhost g) (statusGhost g))
       Pinky  -> color (colorPicker rose (positionGhost g)(statusGhost g))
       Inky   -> color (colorPicker blue (positionGhost g)(statusGhost g))
       Clyde  -> color (colorPicker orange (positionGhost g)(statusGhost g))
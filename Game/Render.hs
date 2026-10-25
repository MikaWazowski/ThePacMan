module Game.Render where

import Game.Types
import Graphics.Gloss

-- Needs to be float since the gloss functions expect float numbers, not integers
tileSize :: Float
tileSize = 48

render :: GameState -> Picture
render state = pictures (grid ++ wallPictures ++ [pacPicture])
  where
    gameBoard = board state
    size = boardDimensions gameBoard
    -- We use a 0 based grid since that is easier for future calculations.
    grid = [drawTile size (x, y) (color blue (rectangleWire tileSize tileSize)) | x <- [0 .. size - 1], y <- [0 .. size - 1]]

    wallPictures = concatMap (drawWall size) (walls gameBoard)

    pacPicture = drawTile size (position (pacMan state)) (rotate angle (pictures [body, mouth, eye]))
      where
        radius = tileSize * 0.36
        body = color yellow (circleSolid radius)
        mouth = color black (polygon [(0, 0), (radius, radius / 2), (radius, -radius / 2)])
        eye = translate (-radius * 0.15) eyeY (color black (circleSolid (radius * 0.2)))

        eyeY = case directionWant (pacMan state) of
          West -> -radius * 0.45
          _    ->  radius * 0.45

        angle = case directionWant (pacMan state) of
          East  -> 0
          South -> 90
          West  -> 180
          North -> 270
        

drawWall :: Int -> Wall -> [Picture]
drawWall size wall =
  [drawTile size (x, wallY) (color blue (rectangleSolid tileSize tileSize)) | x <- [wallStartX .. wallEndX]]
  where
    (wallStartX, wallY) = startPosition wall
    wallEndX = wallStartX + Game.Types.length wall - 1



drawTile :: Int -> Position -> Picture -> Picture
drawTile size (x, y) picture = translate screenX screenY picture
  where
    -- size * tileSize / 2 centers the board to be in the middle of the window
    -- tileSize / 2 makes sure that the starting point of drawing the rectangle is also in the middle and not on the edge of the first tile (what it would be if we didn't add this)
    screenX = fromIntegral x * tileSize - fromIntegral size * tileSize / 2 + tileSize / 2
    screenY = fromIntegral y * tileSize - fromIntegral size * tileSize / 2 + tileSize / 2

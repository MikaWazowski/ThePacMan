module Game.Logic where
import Game.Types


stepPosition :: Direction -> Position -> Position
stepPosition North (x,y) = (x,y+1)
stepPosition South (x,y) = (x, y-1)
stepPosition East (x,y) = (x+1, y)
stepPosition West (x,y) = (x-1,y)

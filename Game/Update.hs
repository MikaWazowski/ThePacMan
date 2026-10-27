module Game.Update where

import Game.Types

step :: Float -> GameState -> GameState
step time state
  | statusGame state /= Ongoing = state -- The game is not running so no timer has to be run
  | otherwise =
        updateScore time state


updateScore :: Float -> GameState -> GameState
updateScore time state = 
    state { timerOngoing = newTime, score = score state + addedScore}   
    where
        oldTime = timerOngoing state
        newTime = oldTime + time
        addedScore = floor newTime - floor oldTime
# ThePacMan

You can move PacMan by either using WASD or the arrows on your keyboard. The goal is to get as many points as possible. 
Colliding with a ghost ends the game. Press Space to start over
Pause the game by pressing P. Resume it by pressing Space
On game over, the best score is saved in `highscore.txt`

Press C to open the custom-level menu. Press Space to continue playing, or C again
to create a new level. In the editor, left-click a cell to add/remove a wall and
right-click to add/remove a power pellet. Press E to export and play the new level.

## Run
To start the program run:

```sh
cabal run pacman
```

## Files
- `Game/Types.hs` defines all types used in the game
- `Game/Model.hs` has game state and rules (initial state, movement, scoring, and updates)
- `Game/GhostAI.hs` contains different algorithms of the ghosts.
- `Game/View.hs` draws the board and game screens
- `Game/Controller.hs` handles input and custom-level file operations
- `main.hs` connects the model, view, and controller

# ThePacMan

You can move PacMan by either using WASD or the arrows on your keyboard. The goal is to get as many points as possible. 
Colliding with a ghost ends the game. Press Space to start over
Pause the game by pressing P. Resume it by pressing Space
On game over, the best score is saved in `highscore.txt`

## Play level
To choose a different level press 1, 2 or 3. To make these levels we used our own custom level creator (therefore the names walls1.txt, walls2.txt and walls3.txt)
For the initial level we needed a list of walls and dots. Therefore we first created a level through our own editor, but since we didn't want to type over all coordinates which belong to both the walls and the power pallets into the list of the initial state, we gave GPT-6 Sol our generates walls0.txt file and asked it to convert it into a list of coordinates.

## Create a level
- Press C to open the custom-level menu. 
- Press Space to continue playing, or C again to create a new level. 
- In the editor, left-click a cell to add a wall, double click to remove a wall
- Right-click to add/remove a power pellet. 
- Press E to export and play the new level.

The first level you created will be saved under key 4
The second level you created will be saved under key 5

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

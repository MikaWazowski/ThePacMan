# ThePacMan

You can move PacMan by either using WASD or the arrows on your keyboard. The goal is to get as many points as possible. 

## Run
To start the program run:

```sh
cabal run pacman
```

## Files
- `Game/Types.hs` defines all types used in the game
- `Game/Logic.hs` handles movement
- `Game/Controller.hs` maps input to movement
- `Game/Render.hs` draws the board
- `Game/GhostAI.hs` contains different algorithms of the ghosts.
- `main.hs` file that uses all other files

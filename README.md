# ThePacMan

Our application will use Gloss for the drawing of the window and the keyboard input.

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
- `main.hs` file that uses all other files

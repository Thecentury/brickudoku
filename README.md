# Blockudoku

[![Hackage](https://img.shields.io/hackage/v/haskell-blockudoku.svg?logo=haskell)](https://hackage.haskell.org/package/haskell-blockudoku)
[![Stackage Lts](http://stackage.org/package/haskell-blockudoku/badge/lts)](http://stackage.org/lts/package/haskell-blockudoku)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

A terminal interface for Blockudoku game — a hybrid of Sudoku and Tetris.

## To-do

### Must have

- [ ] Score system
- [x] Determine and display game over
- [ ] Undo system
- [x] Highlight 3x3 squares
  - [ ] Highlight them with a background of a different color
- [x] Highlight when blocks would be removed after placing a figure
- [ ] (Cheating) Highlight regions where a figure can be placed
- [x] Start placing a figure not from the (0,0) point but from the first where it can be placed
- [x] Display that a figure cannot be placed at all
- [ ] Better readme
- [ ] Package and publish (to Hackage?)
- [ ] Save and restore the state

### Nice to have

- [ ] Mouse support

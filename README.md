# Brickudoku [![Build Status](https://github.com/thecentury/brickudoku/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/thecentury/brickudoku/actions/workflows/haskell-ci.yml)

[![Hackage](https://img.shields.io/hackage/v/brickudoku.svg?logo=haskell)](https://hackage.haskell.org/package/brickudoku)
[![Stackage Lts](https://stackage.org/package/brickudoku/badge/lts)](https://stackage.org/lts/package/brickudoku)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

A terminal interface for Brickudoku game — a hybrid of Sudoku and Tetris.

Built on top of the [brick](https://github.com/jtdaugherty/brick) library which makes building terminal user interfaces very accessible. Sam Tay also has a nice [tutorial](https://samtay.github.io/posts/introduction-to-brick) that can help you get started.

## Installlation

#### Install from source

First [get stack](https://docs.haskellstack.org/en/stable/#how-to-install). Then
```bash
git clone https://github.com/Thecentury/brickudoku.git
cd brickudoku
stack install brickudoku
```

Windows support is unfortunately questionable.

## Usage

Run the game by simply executing the `brickudoku` command.

## To-do

### Must have

- [x] Score system
- [x] Determine and display game over
- [x] Undo system
- [x] Highlight 3x3 squares
  - [x] Highlight them with a background of a different color
- [x] Highlight when blocks would be removed after placing a figure
- [ ] (Cheating) Highlight regions where a figure can be placed
- [x] Start placing a figure not from the (0,0) point but from the first where it can be placed
- [x] Display that a figure cannot be placed at all
- [x] Rename (to 'Brickudoku'?)
- [ ] Better readme
- [ ] Package and publish (to Hackage?)
- [ ] Save and restore the state

### Nice to have

- [ ] Mouse support

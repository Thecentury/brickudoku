# Brickudoku [![Build Status](https://github.com/thecentury/brickudoku/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/thecentury/brickudoku/actions/workflows/haskell-ci.yml)

[![Hackage](https://img.shields.io/hackage/v/brickudoku.svg?logo=haskell)](https://hackage.haskell.org/package/brickudoku)
[![Stackage Lts](https://stackage.org/package/brickudoku/badge/lts)](https://stackage.org/lts/package/brickudoku)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

A terminal interface for Brickudoku game — a hybrid of Sudoku and Tetris.

<video width=923 autoplay controls loop>
  <source src="https://github.com/thecentury/brickudoku/raw/master/docs/img/brickudoku.mp4" type="video/mp4" />
</video>

Built on top of the [brick](https://github.com/jtdaugherty/brick) library which makes building terminal user interfaces very accessible. Sam Tay also has a nice [tutorial](https://samtay.github.io/posts/introduction-to-brick) that can help you get started.

## Installlation

#### Install from source

First [get stack](https://docs.haskellstack.org/en/stable/#how-to-install). Then
```bash
git clone https://github.com/Thecentury/brickudoku.git
cd brickudoku
stack build
# run using stack
stack run
# alternatively, copy the executable to ~/.local/bin
stack install brickudoku
brickudoku
```

Windows support is unfortunately questionable, but you can try building it in [WSL](https://learn.microsoft.com/en-us/windows/wsl/install).

## Usage

Run the game by simply executing the `brickudoku` command.

## To-do

### Must have

- [ ] Package and publish (to Hackage?)
- [ ] Save and restore the state

### Nice to have

- [ ] Mouse support
- [ ] Store the RNG in the game state

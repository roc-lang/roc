# Roc SDL

A platform to use SDL in Roc.

## How to run?

### MacOS

From the root roc directory run:

```shell
brew install sdl2

cp -r $(brew --prefix sdl2)/include examples/sdl/platform

cp -r $(brew --prefix sdl2)/lib examples/sdl/platform

cargo run examples/sdl/FlappyBird.roc
```

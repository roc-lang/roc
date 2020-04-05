#!/bin/sh

DIR="$HOME/.oh-my-zsh/custom/themes"
mkdir -p $DIR
cd $DIR

sed "s/→/➜/g;s/×/✗/g" $HOME/.oh-my-zsh/themes/robbyrussell.zsh-theme > robbyrussell.zsh-theme

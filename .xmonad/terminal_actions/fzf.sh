#!/bin/zsh

. ~/.zshrc

tail -F $1 | fzf --with-nth=2.. | awk '{print $1}'> $2

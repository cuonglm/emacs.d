#!/bin/sh

if [ ! -e ~/.cask ]; then
  echo "Cloning Cask repo"
  git clone git@github.com:cask/cask.git ~/.cask
fi

case $SHELL in
   (*zsh) rcfile=~/.zshrc  ;;
   (*ksh) rcfile=~/.kshrc  ;;
  (*bash) rcfile=~/.bashrc ;;
      (*) echo Not supported shell && exit 1 ;;
esac

if ! grep -q "cask/bin" "$rcfile"; then
  printf 'export PATH=$HOME/.cask/bin:$PATH\n' >> "$rcfile"
fi

export PATH="$HOME/.cask/bin:$PATH"

cd ~/.emacs.d
cask install

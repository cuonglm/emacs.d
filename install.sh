#!/bin/bash

if [[ ! -e ~/.cask ]]
then
    echo "Cloning Cask repo"
    git clone git@github.com:cask/cask.git ~/.cask
fi

if ! grep -q "cask/bin" ~/.bashrc
then
    echo "export PATH=\$HOME/.cask/bin:\$PATH" >> ~/.bashrc
fi

export PATH=$HOME/.cask/bin:$PATH

cd ~/.emacs.d
cask install

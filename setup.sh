#!/bin/sh
dotfiles_dir="$(cd "$(dirname "$0")" && pwd)"

ln -sf $dotfiles_dir/gitconfig $HOME/.gitconfig
ln -sf $dotfiles_dir/gitignore $HOME/.gitignore
ln -sf $dotfiles_dir/emacs $HOME/.emacs.d/init.el

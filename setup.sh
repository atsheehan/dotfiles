#!/bin/sh
dotfiles_dir="$(cd "$(dirname "$0")" && pwd)"

ln -sf $dotfiles_dir/gemrc $HOME/.gemrc
ln -sf $dotfiles_dir/sbtconfig $HOME/.sbtconfig
ln -sf $dotfiles_dir/gitconfig $HOME/.gitconfig
ln -sf $dotfiles_dir/gitignore $HOME/.gitignore

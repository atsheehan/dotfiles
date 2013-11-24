#!/bin/sh
dotfiles_dir="$(cd "$(dirname "$0")" && pwd)"

ln -sf $dotfiles_dir/gemrc $HOME/.gemrc
ln -sf $dotfiles_dir/sbtconfig $HOME/.sbtconfig

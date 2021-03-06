#!/bin/sh
dotfiles_dir="$(cd "$(dirname "$0")" && pwd)"

ln -sf $dotfiles_dir/gemrc $HOME/.gemrc
ln -sf $dotfiles_dir/Xmodmap $HOME/.Xmodmap
ln -sf $dotfiles_dir/sbtconfig $HOME/.sbtconfig
ln -sf $dotfiles_dir/gitconfig $HOME/.gitconfig
ln -sf $dotfiles_dir/gitignore $HOME/.gitignore
ln -sf $dotfiles_dir/emacs $HOME/.emacs
ln -sf $dotfiles_dir/screenrc $HOME/.screenrc
ln -sf $dotfiles_dir/xsession $HOME/.xsession
ln -sf $dotfiles_dir/zshrc $HOME/.zshrc

mkdir -p $HOME/.config
ln -sf $dotfiles_dir/awesome $HOME/.config/awesome

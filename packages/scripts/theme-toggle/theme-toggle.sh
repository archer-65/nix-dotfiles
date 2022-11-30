#!/usr/bin/env bash

dotfiles=$HOME/.dotfiles
path=${dotfiles}/home/configurations/mario@${HOSTNAME}/default.nix

if [[ $(grep darkTheme $path) =~ false ]]; then
  subst=true
else
  subst=false
fi

sed -i "s/darkTheme.*=.*/darkTheme = ${subst};/g" $path

home-manager switch --flake ${dotfiles}#mario@${HOSTNAME}


command -v emacs > /dev/null || { echo "Emacs missing."; exit 1; }

ps -ef | grep -e emacsclient | grep -v grep > /dev/null || exit 1

case "$subst"
in
    true) emacsclient -e "(modus-themes-load-vivendi)" > /dev/null ;;
    false) emacsclient -e "(modus-themes-load-operandi)" > /dev/null ;;
esac

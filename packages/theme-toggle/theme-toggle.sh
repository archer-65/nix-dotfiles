#!/usr/bin/env bash

dotfiles=$HOME/.dotfiles
path=$HOME/.dotfiles/home/configurations/mario@${HOSTNAME}/default.nix

toggle_emacs() {
    command -v emacs > /dev/null || { echo "Emacs missing."; return 1; }
    ps -ef | grep -e emacsclient | grep -v grep > /dev/null || return 1

    case $1 in
        true) emacsclient -e "(modus-themes-load-vivendi)" > /dev/null ;;
        false) emacsclient -e "(modus-themes-load-operandi)" > /dev/null ;;
    esac
}

if [[ $(grep darkTheme $path) =~ false ]]; then
    style=true
else
    style=false
fi

sed -i "s/darkTheme.*=.*/darkTheme = ${style};/g" $path

home-manager switch --flake ${dotfiles}#mario@${HOSTNAME}

toggle_emacs $style

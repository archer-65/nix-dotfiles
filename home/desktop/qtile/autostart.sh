#!/usr/bin/env bash

# Configure X and KB
#xrandr --rate 144.00
autorandr --change
setxkbmap -layout us -variant intl

# Polkit
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

# Lockscreen manager
xss-lock -l -- betterlockscreen -l blur --off 300 &

# CoreCtrl
if command -v corectrl &> /dev/null ; then
	corectrl &
fi

# Compositor
picom -b

# Wallpaper
nitrogen --restore &

# Notifications
dunst &

# Screenshots background
flameshot &

#!/usr/bin/env bash

# Wallpaper
nitrogen --restore &

# Configure X and KB
#xrandr --rate 144.00
autorandr --change

# Polkit
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

# Audio tray
pasystray &

# Lockscreen manager
xss-lock -l -- betterlockscreen -l blur --off 300 &

# CoreCtrl
if command -v corectrl &> /dev/null ; then
	corectrl &
fi

# Compositor
# picom -b

# Notifications
# dunst &

# Screenshots background
# flameshot &

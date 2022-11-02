#!/usr/bin/env bash

# pkgs.writeShellScriptBin "rofi-powermenu" ''
rofi_dir="$HOME/.config/rofi"
dir="$rofi_dir/themes"

theme="$dir/powermenu"
confirm="$dir/confirm"
message="$dir/message"

rofi_command="rofi -theme $theme"

uptime=$(uptime -p | sed -e 's/up //g')
cpu=$(usedcpu)
memory=$(usedram)

# Options
shutdown=""
reboot="勒"
lock=""
suspend=""
logout=""

# Confirmation
confirm_exit() {
    rofi -dmenu\
    -i\
    -no-fixed-num-lines\
    -p "Are You Sure?  "\
    -theme $confirm
}

# Message
msg() {
    rofi -theme $message -e "Available Options - Y / N"
}

# Variable passed to rofi
options="$shutdown\n$reboot\n$lock\n$suspend\n$logout"

chosen="$(echo -e "$options" | $rofi_command -p "   $uptime  |    $cpu  |  ﬙  $memory " -dmenu -selected-row 2)"
case $chosen in
    $shutdown)
        ans=$(confirm_exit &)
        if [[ $ans == "Y" ]]; then
            systemctl poweroff
            elif [[ $ans == "N" ]]; then
            exit 0
        else
            msg
        fi
    ;;
    $reboot)
        ans=$(confirm_exit &)
        if [[ $ans == "Y" ]]; then
            systemctl reboot
            elif [[ $ans == "N" ]]; then
            exit 0
        else
            msg
        fi
    ;;
    $lock)
        loginctl lock-session
    ;;
    $suspend)
        ans=$(confirm_exit &)
        if [[ $ans == "Y" ]]; then
            systemctl suspend
            elif [[ $ans == "N" ]]; then
            exit 0
        else
            msg
        fi
    ;;
    $logout)
        ans=$(confirm_exit &)
        if [[ $ans == "Y" ]]; then
            if [[ "$DESKTOP_SESSION" =~ "qtile" ]]; then
                qtile cmd-obj -o cmd -f shutdown
            elif [[ "$DESKTOP_SESSION" =~ "i3" ]]; then
                i3-msg exit
	    elif [[ "$XDG_CURRENT_DESKTOP" =~ "sway" ]]; then
		swaymsg exit
            fi
            elif [[ $ans == "N" ]]; then
            exit 0
        else
            msg
        fi
    ;;
esac

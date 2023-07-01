#!/usr/bin/env bash

# Configurations folder
themes_dir="$HOME/.config/rofi/themes"
theme="$themes_dir/powermenu"
confirm="$themes_dir/confirm"
message="$themes_dir/message"

uptime_command=$(uptime -p | sed -e 's/up //g')
rofi_command="rofi -theme $theme"

# Options
shutdown=""
reboot=""
lock=""
suspend=""
logout="󰍃"

# Convenience
execute_with_confirm() {
    ans=$(confirm_exit &)
    if [[ $ans == "Y" ]]; then
        $1
    elif [[ $ans == "N" ]]; then
        exit 0
    else
        msg
    fi
}

# Confirmation prompt
confirm_exit() {
    rofi -dmenu \
        -i \
        -no-fixed-num-lines \
        -p "Are You Sure?  " \
        -theme $confirm
}

# Message due to wrong input
msg() {
    rofi -theme $message -e "Available Options - Y / N"
}

shutdown() {
    execute_with_confirm "systemctl poweroff"
}

reboot() {
    execute_with_confirm "systemctl reboot"
}

suspend() {
    execute_with_confirm "systemctl suspend"
}

logout() {
    if [[ "$DESKTOP_SESSION" =~ "qtile" ]]; then
        execute_with_confirm "qtile cmd-obj -o cmd -f shutdown"
    elif [[ "$DESKTOP_SESSION" =~ "i3" ]]; then
        execute_with_confirm "i3-msg exit"
    elif [[ "$XDG_CURRENT_DESKTOP" =~ "sway" ]]; then
        execute_with_confirm "swaymsg exit && systemctl stop --user sway-session.target"
    elif [[ "$XDG_CURRENT_DESKTOP" =~ "Hyprland" ]]; then
        execute_with_confirm "hyprctl dispatch -- exit && systemctl stop --user hyprland-session.target"
    fi
}

# Variable passed to rofi
options="${shutdown}\n${reboot}\n${lock}\n${suspend}\n${logout}"

chosen="$(echo -e "$options" | $rofi_command -p " Uptime: $uptime_command " -dmenu -selected-row 2)"

case $chosen in
    $shutdown) shutdown ;;
    $reboot) reboot ;;
    $lock) loginctl lock-session ;;
    $suspend) suspend ;;
    $logout) logout ;;
esac

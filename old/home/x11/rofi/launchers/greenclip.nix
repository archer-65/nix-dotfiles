{ pkgs, ... }:

pkgs.writeShellScriptBin "rofi_clipboard" ''
  dir="$HOME/.config/rofi/themes/greenclip"

  greenclip_copy() {
      rofi -modi "clipboard:greenclip print" \
          -show clipboard \
          -run-command '{cmd}' \
          -theme $dir
  }

  greenclip_paste() {
      greenclip_copy 
      sleep 0.5
      xclip -o selection clipboard
      xdotool key shift+Insert
  }

  greenclip_error() {
      dunstify -u critical "Greenclip script error!"
  }

  case $1 in
      copy)
          greenclip_copy
          ;;
      paste)
          greenclip_paste
          ;;
      *)
          greenclip_error
          ;;
  esac
''

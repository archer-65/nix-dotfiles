{ pkgs, ... }:
let rofi = "${pkgs.rofi}/bin/rofi";
in pkgs.writeShellScriptBin "rofi_launcher" ''
  dir="$HOME/.config/rofi/themes/launcher"

  if [[ $XDG_SESSION_TYPE == "wayland" ]]; then
    modi="-modi run,drun"
  else
    modi="-modi run,drun,window"
  fi

  ${rofi} -no-lazy-grab -show drun \
  $modi \
  -theme $dir

  #-modi run,drun,window \
''

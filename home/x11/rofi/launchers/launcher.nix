{ pkgs, ... }:

pkgs.writeShellScriptBin "rofi_launcher" '' 
  dir="$HOME/.config/rofi/themes/launcher"

  rofi -no-lazy-grab -show drun \
  -modi run,drun,window \
  -theme $dir
''

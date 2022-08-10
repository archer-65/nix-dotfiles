{ pkgs, ... }:
let
  # Not working, idk
  # rofi = "${pkgs.rofi}/bin/rofi";
in pkgs.writeShellScriptBin "rofi_emoji" ''
  dir="$HOME/.config/rofi/themes/emoji"

  rofi -show emoji \
  -modi emoji \
  -theme $dir 
''

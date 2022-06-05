{ pkgs, ... }:
let rofi = "${pkgs.rofi}/bin/rofi";
in pkgs.writeShellScriptBin "rofi_launcher" ''
  dir="$HOME/.config/rofi/themes/emoji"

  ${rofi} -no-lazy-grab -show drun \
  -modi run,drun,window \
  -theme $dir
''

{ pkgs, ... }:

pkgs.writeShellScriptBin "rofi_emoji" ''
  dir="$HOME/.config/rofi/themes/emoji"

  rofi -show emoji \
      -modi emoji \
      -theme $dir
''

{pkgs, ...}:
pkgs.writeShellScriptBin "rofi-emoji" ''
  dir="$HOME/.config/rofi/themes/emoji"

  rofi -show emoji \
  -modi emoji \
  -theme $dir
''

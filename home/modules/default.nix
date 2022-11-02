{utils}:
utils.lib.exportModules [
  ./credentials/gpg.nix
  ./credentials/mail.nix
  ./credentials/bitwarden.nix

  ./desktop/xorg
  ./desktop/xorg/qtile.nix

  ./desktop/wayland
  ./desktop/wayland/sway.nix
  ./desktop/wayland/hyprland.nix

  ./desktop/apps/greenclip.nix
  ./desktop/apps/rofi.nix
  ./desktop/apps/autorandr.nix
  ./desktop/apps/discord.nix
  ./desktop/apps/teams.nix

  ./desktop/browsers/firefox.nix
  ./desktop/browsers/chromium.nix

  ./desktop/gaming/emulators.nix

  ./desktop/media/documents.nix
  ./desktop/media/videos.nix

  ./desktop/term/alacritty.nix

  ./desktop/services/picom.nix
  ./desktop/services/dunst.nix
  ./desktop/services/locker.nix
  ./desktop/services/locker-wayland.nix
  ./desktop/services/waybar.nix

  ./dev/cc.nix
  ./dev/js
  ./dev/java.nix
  ./dev/nix-lang.nix
  ./dev/rust.nix
  ./dev/terraform.nix
  ./dev/tex.nix

  ./editors/android-studio.nix
  ./editors/emacs.nix
  ./editors/intellij.nix
  ./editors/neovim.nix
  ./editors/vscode.nix

  ./shell/bash.nix
  ./shell/direnv.nix
  ./shell/extensions.nix
  ./shell/git.nix
  ./shell/starship.nix

  ./themes/options.nix
  ./themes/onedark
]

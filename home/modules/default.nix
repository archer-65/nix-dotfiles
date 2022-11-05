{
  "credentials/gpg" = import ./credentials/gpg.nix;
  "credentials/mail" = import ./credentials/mail.nix;
  "credentials/bitwarden" = import ./credentials/bitwarden.nix;
  "credentials/yubikey" = import ./credentials/yubikey.nix;

  "desktop/xorg" = import ./desktop/xorg;
  "desktop/xorg/qtile" = import ./desktop/xorg/qtile.nix;

  "desktop/wayland" = import ./desktop/wayland;
  "desktop/wayland/sway" = import ./desktop/wayland/sway.nix;
  "desktop/wayland/hyprland" = import ./desktop/wayland/hyprland.nix;

  "desktop/apps/greenclip" = import ./desktop/apps/greenclip.nix;
  "desktop/apps/rofi" = import ./desktop/apps/rofi.nix;
  "desktop/apps/autorandr" = import ./desktop/apps/autorandr.nix;
  "desktop/apps/discord" = import ./desktop/apps/discord.nix;
  "desktop/apps/teams" = import ./desktop/apps/teams.nix;

  "desktop/browsers/firefox" = import ./desktop/browsers/firefox.nix;
  "desktop/browsers/chromium" = import ./desktop/browsers/chromium.nix;

  "desktop/gaming/emulators" = import ./desktop/gaming/emulators.nix;

  "desktop/media/documents" = import ./desktop/media/documents.nix;
  "desktop/media/videos" = import ./desktop/media/videos.nix;

  "desktop/term/alacritty" = import ./desktop/term/alacritty.nix;

  "desktop/services/picom" = import ./desktop/services/picom.nix;
  "desktop/services/dunst" = import ./desktop/services/dunst.nix;
  "desktop/services/locker" = import ./desktop/services/locker.nix;
  "desktop/services/locker-wayland" = import ./desktop/services/locker-wayland.nix;
  "desktop/services/waybar" = import ./desktop/services/waybar.nix;

  "dev/cc" = import ./dev/cc.nix;
  "dev/js" = import ./dev/js;
  "dev/java" = import ./dev/java.nix;
  "dev-lang.nix" = import ./dev/nix-lang.nix;
  "dev/rust" = import ./dev/rust.nix;
  "dev/terraform" = import ./dev/terraform.nix;
  "dev/tex" = import ./dev/tex.nix;

  "editors/android-studio" = import ./editors/android-studio.nix;
  "editors/emacs" = import ./editors/emacs.nix;
  "editors/intellij" = import ./editors/intellij.nix;
  "editors/neovim" = import ./editors/neovim.nix;
  "editors/vscode" = import ./editors/vscode.nix;

  "shell/bash" = import ./shell/bash.nix;
  "shell/direnv" = import ./shell/direnv.nix;
  "shell/extensions" = import ./shell/extensions.nix;
  "shell/git" = import ./shell/git.nix;
  "shell/starship" = import ./shell/starship.nix;

  "themes/options" = import ./themes/options.nix;
  "themes/onedark" = import ./themes/onedark;
}

inputs: {
  "credentials/gpg" = import ./credentials/gpg.nix inputs;
  "credentials/mail" = import ./credentials/mail.nix inputs;

  "desktop/xorg" = import ./desktop/xorg inputs;
  "desktop/xorg/qtile" = import ./desktop/xorg/qtile.nix inputs;

  "desktop/wayland" = import ./desktop/wayland inputs;
  "desktop/wayland/sway" = import ./desktop/wayland/sway.nix inputs;

  "desktop/apps/greenclip" = import ./desktop/apps/greenclip.nix inputs;
  "desktop/apps/rofi" = import ./desktop/apps/rofi.nix inputs;
  "desktop/apps/autorandr" = import ./desktop/apps/autorandr.nix inputs;
  "desktop/apps/discord" = import ./desktop/apps/discord.nix inputs;
  "desktop/apps/teams" = import ./desktop/apps/teams.nix inputs;

  "desktop/browsers/firefox" = import ./desktop/browsers/firefox.nix inputs;
  "desktop/browsers/chromium" = import ./desktop/browsers/chromium.nix inputs;

  "desktop/media/documents" = import ./desktop/media/documents.nix inputs;

  "desktop/term/alacritty" = import ./desktop/term/alacritty.nix inputs;

  "desktop/services/picom" = import ./desktop/services/picom.nix inputs;
  "desktop/services/dunst" = import ./desktop/services/dunst.nix inputs;
  "desktop/services/locker" = import ./desktop/services/locker.nix inputs;
  "desktop/services/locker-wayland" = import ./desktop/services/locker-wayland.nix inputs;
  "desktop/services/waybar" = import ./desktop/services/waybar.nix inputs;

  "/dev/c-c++" = import ./dev/cc.nix inputs;
  "/dev/javascript" = import ./dev/js inputs;
  "/dev/java" = import ./dev/java.nix inputs;
  "/dev/nix-lang" = import ./dev/nix-lang.nix inputs;
  "/dev/tex" = import ./dev/tex.nix inputs;

  "editors/android-studio" = import ./editors/android-studio.nix inputs;
  "editors/emacs" = import ./editors/emacs.nix inputs;
  "editors/intellij" = import ./editors/intellij.nix inputs;
  "editors/neovim" = import ./editors/neovim.nix inputs;
  "editors/vscode" = import ./editors/vscode.nix inputs;

  "shell/bash" = import ./shell/bash.nix inputs;
  "shell/direnv" = import ./shell/direnv.nix inputs;
  "shell/extensions" = import ./shell/extensions.nix inputs;
  "shell/git" = import ./shell/git.nix inputs;
  "shell/starship" = import ./shell/starship.nix inputs;

  "themes/options" = import ./themes/options.nix inputs;
  "themes/materia" = import ./themes/materia inputs;

  home = import ./home.nix inputs;
}

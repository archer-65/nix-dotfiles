inputs: {
  "desktop/qtile" = import ./desktop/qtile.nix inputs;
  "desktop/apps/greenclip" = import ./desktop/apps/greenclip.nix inputs;
  "desktop/apps/rofi" = import ./desktop/apps/rofi.nix inputs;
  "desktop/term/alacritty" = import ./desktop/term/alacritty.nix inputs;
  "desktop/services/picom" = import ./desktop/services/picom.nix inputs;
  "desktop/services/dunst" = import ./desktop/services/dunst.nix inputs;

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
  "shell/gpg" = import ./shell/gpg.nix inputs;

  "themes/gtk" = import ./themes/gtk.nix inputs;
  "themes/materia" = import ./themes/materia inputs;

  home = import ./home.nix inputs;
}

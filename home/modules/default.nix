inputs: {
  "desktop/services/picom" = import ./desktop/services/picom.nix inputs;
  "desktop/services/dunst" = import ./desktop/services/dunst.nix inputs;

  "editors/android-studio" = import ./editors/android-studio.nix inputs;
  "editors/emacs" = import ./editors/emacs.nix inputs;
  "editors/intellij" = import ./editors/intellij.nix inputs;
  "editors/neovim" = import ./editors/neovim.nix inputs;
  "editors/vscode" = import ./editors/vscode.nix inputs;

  "themes/gtk" = import ./themes/gtk.nix inputs;
  "themes/materia" = import ./themes/materia inputs;

  home = import ./home.nix inputs;
}

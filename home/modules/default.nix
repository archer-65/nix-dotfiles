inputs: {
  "editors/android-studio" = import ./editors/android-studio.nix inputs;
  "editors/emacs" = import ./editors/emacs.nix inputs;
  "editors/intellij" = import ./editors/intellij.nix inputs;
  "editors/neovim" = import ./editors/neovim.nix inputs;
  "editors/vscode" = import ./editors/vscode.nix inputs;
}
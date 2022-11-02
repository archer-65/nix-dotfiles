{pkgs}:
pkgs.mkShell {
  # Shell name
  name = "dotfiles";

  # Dependencies
  nativeBuildInputs = with pkgs; [
    nix
    git
    home-manager
  ];

  # Environment
  NIX_CONFIG = "extra-experimental-features = nix-command flakes repl-flake";
}

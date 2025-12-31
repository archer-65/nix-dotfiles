{...}: {
  homebrew = {
    enable = true;

    onActivation = {
      autoUpdate = true;
      # cleanup = "zap"; # Headaches, continuously reinstalling docker cask...
      upgrade = true;
    };

    taps = [
      "outscale/tap"
    ];

    brews = [
      # Move to Outscale module
      "outscale/tap/oapi-cli"

      # Move to Vault module (switch to Nix where possible)
      "vault"
      "vault-token-helper"

      # Less relevant but still
      "octopus-cli"
    ];

    casks = [
      "firefox"
      "docker-desktop"
      "microsoft-teams"
      "microsoft-excel"
      "tunnelblick"
      "keybase"
      "karabiner-elements"
      "windows-app"

      # Move to Terraform module
      "suzuki-shunsuke/tfmv/tfmv"
   ];
  };
}

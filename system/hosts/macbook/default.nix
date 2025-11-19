{...}: {
  homebrew = {
    enable = true;

    onActivation = {
      autoUpdate = true;
      # cleanup = "zap"; # Headaches, continuously reinstalling docker cask...
      upgrade = true;
    };

    casks = [
      "firefox"
      "docker-desktop"
      "microsoft-teams"
      "microsoft-excel"
      "tunnelblick"
      "keybase"
      "karabiner-elements"
      "windows-app"
      "suzuki-shunsuke/tfmv/tfmv"
    ];
  };
}

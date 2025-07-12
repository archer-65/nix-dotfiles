{...}: {
  homebrew = {
    enable = true;

    onActivation = {
      cleanup = "zap";
    };

    casks = [
      "firefox"
      "docker"
      "microsoft-teams"
      "microsoft-excel"
      "tunnelblick"
      "keybase"
      "karabiner-elements"
    ];
  };
}

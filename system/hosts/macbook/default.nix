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
      "karabiner-elements"

      # Move to Terraform module
      "suzuki-shunsuke/tfmv/tfmv"
    ];
  };

  system.defaults.NSGlobalDomain.InitialKeyRepeat = 10;
  system.defaults.NSGlobalDomain.KeyRepeat = 2;
  system.defaults.NSGlobalDomain."com.apple.keyboard.fnState" = true;
}

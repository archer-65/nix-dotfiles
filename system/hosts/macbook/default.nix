{config, ...}: {
  # TODO: Move this and homebrew to dedicated module
  environment.variables = {
    HOMEBREW_BAT = "1";
    HOMEBREW_NO_ANALYTICS = "1";
    HOMEBREW_NO_INSECURE_REDIRECT = "1";
  };
  environment.systemPath = ["${config.homebrew.prefix}/bin"];

  homebrew = {
    enable = true;
    enableBashIntegration = true;
    enableZshIntegration = true;

    global = {
      brewfile = true;
      autoUpdate = true;
    };

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
      {
        name = "suzuki-shunsuke/tfmv/tfmv";
        trusted = true;
      }
    ];
  };

  system.defaults.NSGlobalDomain.InitialKeyRepeat = 10;
  system.defaults.NSGlobalDomain.KeyRepeat = 2;
  system.defaults.NSGlobalDomain."com.apple.keyboard.fnState" = true;
}

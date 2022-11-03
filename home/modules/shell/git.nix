{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.home.modules.shell.git-defaults;
  cfgSign = config.home.modules.credentials.gpg.signing;
in {
  options.home.modules.shell.git-defaults = {
    enable = mkEnableOption "main user git configuration";
  };

  config = mkIf cfg.enable {
    programs.git = {
      enable = true;
      userEmail = "mario.liguori.056@gmail.com";
      userName = "archer-65";

      signing = mkIf cfgSign.enable {
        key = cfgSign.key;
        signByDefault = true;
      };
    };
  };
}

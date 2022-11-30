{
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.shell.git-defaults;
in {
  options.mario.modules.shell.git-defaults = {
    enable = mkEnableOption "main user git configuration";
  };

  config = mkIf cfg.enable {
    programs.git = {
      enable = true;
      userEmail = "mario.liguori.056@gmail.com";
      userName = "archer-65";

      signing = {
        key = "BAC570B2172822A3";
        signByDefault = true;
      };
    };
  };
}

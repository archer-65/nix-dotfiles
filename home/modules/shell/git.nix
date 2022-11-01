{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.user-modules.shell.git-defaults;
in {
  options.user-modules.shell.git-defaults = {
    enable = mkEnableOption "main user git configuration";
  };

  config = mkIf cfg.enable {
    programs.git = {
      enable = true;
      userEmail = "mario.liguori.056@gmail.com";
      userName = "archer-65";
    };
  };
}

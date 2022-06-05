_:
{ options, config, lib, ... }:

with lib;
let cfg = config.user-modules.shell.git-defaults;
in {
  options.user-modules.shell.git-defaults = with types; {
    enable = mkOption {
      default = false;
      type = bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    programs.git = {
      enable = true;
      userEmail = "mariogt2009@live.it";
      userName = "archer-65";
    };
  };
}

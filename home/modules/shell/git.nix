_:
{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.user-modules.shell.git;
in {
  options.user-modules.shell.git = with types; {
    enable = mkOption {
      default = false;
      type = bool;
      example = true;
    };

    email = mkOption {
      default = null;
      type = nullOr str;
      example = "example@gmail.com";
    };

    user = mkOption {
      default = null;
      type = nullOr str;
      example = "pippo";
    };
  };

  config = mkIf cfg.enable {
    programs.git = {
      enable = true;
      userEmail = cfg.email;
      userName = cfg.user;
      #userEmail = "mariogt2009@live.it";
      #userName = "archer-65";
    };
  };
}

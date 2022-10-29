_:
{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.user-modules.dev.java;
in {
  options.user-modules.dev.java = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    programs.java.enable = true;

    home.packages = with pkgs; [ maven gradle ];
  };
}

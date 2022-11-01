{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.user-modules.dev.java;
in {
  options.user-modules.dev.java = {
    enable = mkEnableOption "java full support (maven and gradle included)";
  };

  config = mkIf cfg.enable {
    programs.java.enable = true;

    home.packages = with pkgs; [ maven gradle ];
  };
}

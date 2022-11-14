{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.dev.java;
in {
  options.mario.modules.dev.java = {
    enable = mkEnableOption "java full support (maven and gradle included)";
  };

  config = mkIf cfg.enable {
    programs.java.enable = true;

    home.packages = with pkgs; [maven gradle];
  };
}

{
  pkgs,
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.dev.docker;
in {
  options.system.modules.dev.docker = {
    enable = mkEnableOption "docker configuration";
  };

  config = mkIf cfg.enable {
    virtualisation.docker = {enable = true;};
    environment.systemPackages = [ pkgs.docker-compose ];
    primaryUser.extraGroups = ["docker"];
  };
}

{
  config,
  options,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.credentials.ssh;
in {
  options.mario.modules.credentials.ssh = {
    enable = mkEnableOption "ssh user configuration";
  };

  config = mkIf cfg.enable {
    sops.secrets."work/config" = {
      sopsFile = ./work/config;
      mode = "0400";
      format = "binary";
      path = "${config.home.homeDirectory}/.ssh/work/config";
    };

    sops.secrets."work/plugin" = {
      sopsFile = ./work/plugin;
      mode = "0400";
      format = "binary";
      path = "${config.home.homeDirectory}/.ssh/config.plugin";
    };

    sops.secrets."work/ssh-route" = {
      sopsFile = ./work/ssh-route;
      mode = "0500";
      format = "binary";
      path = "${config.home.homeDirectory}/.ssh/work/ssh-route";
    };

    programs.ssh.enable = true;

    programs.ssh.includes = [
      "work/config"
    ];
  };
}

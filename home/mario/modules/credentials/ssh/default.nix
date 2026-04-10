{
  pkgs,
  config,
  options,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.credentials.ssh;
  cfgWork = config.mario.modules.mixins.work;
in {
  options.mario.modules.credentials.ssh = {
    enable = mkEnableOption "ssh user configuration";
  };

  config = mkIf cfg.enable {
    systemd.user.tmpfiles.rules = lib.mkIf (pkgs.stdenv.isDarwin == false) [
      "d ${config.home.homeDirectory}/.ssh/sockets - - - - -"
    ];

    home.file = {
      ".ssh/keys.d/yubikey-id_ed25519.pub".text = ''
        ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBR9GjKkCrbAbfuQJXuMTh1I6agrhiHrxlEXhWgidvLS
      '';
    };

    programs.ssh = {
      enable = true;

      enableDefaultConfig = false;
      matchBlocks."*" = {
        forwardAgent = false;
        addKeysToAgent = "no";
        compression = false;
        serverAliveInterval = 0;
        serverAliveCountMax = 3;
        hashKnownHosts = false;
        userKnownHostsFile = "~/.ssh/known_hosts";

        controlMaster = "auto";
        controlPath = "~/.ssh/sockets/%r@%h-%p";
        controlPersist = "60m";
      };

      matchBlocks."forges" = {
        host = "github.com gitlab.com";
        user = "git";
        identitiesOnly = true;
        identityFile = ["~/.ssh/keys.d/yubikey-id_ed25519.pub"];
      };

      matchBlocks."work" = lib.mkIf (cfgWork.enable) {
        host = "github-work";
        hostname = "github.com";
        identitiesOnly = true;
        identityFile = ["~/.ssh/id_ed25519"];
      };
    };
  };
}

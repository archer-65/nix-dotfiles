{
  pkgs,
  config,
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
      settings."*" = {
        ForwardAgent = false;
        AddKeysToAgent = "no";
        Compression = false;
        ServerAliveInterval = 0;
        ServerAliveCountMax = 3;
        HashKnownHosts = false;
        UserKnownHostsFile = "~/.ssh/known_hosts";

        ControlMaster = "auto";
        ControlPath = "~/.ssh/sockets/%r@%n-%p";
        ControlPersist = "60m";
      };

      settings."forges" = {
        host = "github.com gitlab.com";
        user = "git";
        identitiesOnly = true;
        identityFile = ["~/.ssh/keys.d/yubikey-id_ed25519.pub"];
      };

      # NOTE: This way I can use different keys and agents.
      # Different agents are handy for alternative access methods, or
      # if I want to get rid of GPG being the only SSH agent.
      settings."work" = lib.mkIf (cfgWork.enable) {
        host = "github-work";
        hostname = "github.com";
        identitiesOnly = true;
        identityFile = ["~/.ssh/id_ed25519"];
      };
    };
  };
}

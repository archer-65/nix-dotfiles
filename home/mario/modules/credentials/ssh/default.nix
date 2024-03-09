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
    home.file = {
      ".ssh/keys.d/yubikey-id_ed25519.pub".text = ''
        ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBR9GjKkCrbAbfuQJXuMTh1I6agrhiHrxlEXhWgidvLS
      '';
      ".ssh/sockets/.keep".text = "";
    };

    sops.secrets."work/config" = {
      sopsFile = ./work/config.enc;
      mode = "0400";
      format = "binary";
      path = "${config.home.homeDirectory}/.ssh/work/config";
    };

    programs.ssh = {
      enable = true;
      controlMaster = "auto";
      controlPath = "~/.ssh/sockets/%r@%h-%p";
      controlPersist = "60m";

      matchBlocks."forges" = {
        host = "github.com gitlab.*.com";
        user = "git";
        identitiesOnly = true;
        identityFile = [ "~/.ssh/keys.d/yubikey-id_ed25519.pub" ];
      };

      includes = [
        "work/config"
      ];
    };
  };
}

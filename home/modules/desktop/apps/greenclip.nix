_:
{ pkgs, config, lib, options, ... }:

with lib;
let
  configDir = config.dotfiles.configDir;
  cfg = config.user-modules.desktop.apps.greenclip;
in {
  options.user-modules.desktop.apps.greenclip = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      haskellPackages.greenclip
      scripts.rofi.greenclip
    ];

    xdg.configFile."greenclip.cfg".source = "${configDir}/greenclip.toml";

    systemd.user.services.greenclip = {
      Unit = {
        Description = "greenclip daemon";
        After = [ "graphical-session.target" ];
      };
      Install = { WantedBy = [ "graphical-session.target" ]; };
      Service = {
        ExecStart = "${pkgs.haskellPackages.greenclip}/bin/greenclip daemon";
      };
    };
  };
}

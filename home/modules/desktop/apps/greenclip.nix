{
  pkgs,
  config,
  lib,
  options,
  ...
}:
with lib; let
  inherit (config.dotfiles) configDir;
  cfg = config.home.modules.desktop.apps.greenclip;
in {
  options.home.modules.desktop.apps.greenclip = {
    enable = mkEnableOption "greenclip support";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      haskellPackages.greenclip
      rofi-plugins.greenclip
    ];

    xdg.configFile."greenclip.cfg".source = "${configDir}/greenclip.toml";

    systemd.user.services.greenclip = {
      Unit = {
        Description = "greenclip daemon";
        After = ["graphical-session.target"];
      };
      Install = {WantedBy = ["graphical-session.target"];};
      Service = {
        ExecStart = "${pkgs.haskellPackages.greenclip}/bin/greenclip daemon";
      };
    };
  };
}

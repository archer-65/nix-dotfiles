_:
{ pkgs, config, ... }:

let configDir = config.dotfiles.configDir;
in {
  home.packages = with pkgs.haskellPackages; [ greenclip ];

  # xdg.configFile."greenclip.cfg".source = ../../../../config/greenclip.toml;
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
}

{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.home.modules.editors.emacs;
  cfgWayland = config.home.modules.desktop.wayland;
  inherit (config.dotfiles) configDir;
in {
  options.home.modules.editors.emacs = {
    enable = mkEnableOption "emacs and its configuration";
    daemon.enable = mkEnableOption "emacs daemon";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      programs.emacs = {
        enable = true;
        package = pkgs.emacsPgtkNativeComp;

        extraPackages = epkgs: [epkgs.vterm epkgs.melpaPackages.telega];
      };

      xdg.configFile."emacs" = {
        source = "${configDir}/emacs";
        recursive = true;
      };
    }

    (mkIf cfg.daemon.enable {
      services.emacs = {
        enable = true;
        client.enable = true;
      };

      home.sessionVariables = {
        EDITOR = "emacsclient -t";
        VISUAL = "emacsclient -c -a emacs";
      };
    })

    (mkIf (cfgWayland.wm == "sway" && cfg.daemon.enable) {
      systemd.user.services.emacs.Install.WantedBy =
        lib.mkForce ["sway-session.target"];
    })
  ]);
}

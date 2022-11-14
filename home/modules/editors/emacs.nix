{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.mario.modules.editors.emacs;
  cfgWayland = config.mario.modules.desktop.wayland;
  inherit (config.dotfiles) configDir;
in {
  options.mario.modules.editors.emacs = {
    enable = mkEnableOption "emacs and its configuration";
    daemon.enable = mkEnableOption "emacs daemon";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      programs.emacs = {
        enable = true;
        package = pkgs.emacsPgtkNativeComp;

        extraPackages = epkgs: [epkgs.vterm  epkgs.melpaPackages.telega];
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

      systemd.user.services.emacs = {
        # Needed for Wayland sessions
        Unit = {
          After = ["default.target"];
          PartOf = ["default.target"];
        };
      };

      home.sessionVariables = {
        EDITOR = "emacsclient -t";
        VISUAL = "emacsclient -c -a emacs";
      };
    })
  ]);
}

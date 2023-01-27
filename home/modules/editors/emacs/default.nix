{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.editors.emacs;
in {
  options.mario.modules.editors.emacs = {
    enable = mkEnableOption "emacs and its configuration";
    daemon.enable = mkEnableOption "emacs daemon";
    telega.enable = mkEnableOption "telegram client";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = [pkgs.hack-font];

      programs.emacs = {
        enable = true;
        package = pkgs.emacsPgtk;

        extraPackages = epkgs: [
          epkgs.vterm
        ] ++ (optionals cfg.telega.enable [
          epkgs.melpaPackages.telega
        ]);
      };

      xdg.configFile."emacs" = {
        source = ./config;
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

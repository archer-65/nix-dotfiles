{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.mario.modules.editors.emacs;
  configDir = "${config.home.homeDirectory}/.dotfiles/home/modules/editors/emacs/config";
in {

  options.mario.modules.editors.emacs = {
    enable = mkEnableOption "emacs and its configuration";
    daemon.enable = mkEnableOption "emacs daemon";
    telega.enable = mkEnableOption "telegram client";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      nixpkgs.overlays = [ inputs.emacs-overlay.overlays.default ];

      home.packages = [pkgs.hack-font];

      programs.emacs = {
        enable = true;
        package = pkgs.emacsPgtk;

        extraPackages = epkgs:
          [
            epkgs.vterm
          ]
          ++ (optionals cfg.telega.enable [
            epkgs.melpaPackages.telega
          ]);
      };

      xdg.configFile."emacs/Emacs.org".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/Emacs.org";
      xdg.configFile."emacs/init.el".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/init.el";
      xdg.configFile."emacs/early-init.el".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/early-init.el";
      xdg.configFile."emacs/lisp".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/lisp";
      xdg.configFile."emacs/straight/version".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/straight/version";
      xdg.configFile."emacs/img".source = config.lib.file.mkOutOfStoreSymlink "${configDir}/img";
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

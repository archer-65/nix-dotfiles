{ config, lib, pkgs, inputs, ... }:

with lib;
let
  cfg = config.user-modules.editors.emacs;
  cfgWayland = config.user-modules.desktop.wayland;
  inherit (config.dotfiles) configDir;
in {
  options.user-modules.editors.emacs = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
    daemon.enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      programs.emacs = {
        enable = true;
        package = pkgs.emacsPgtkNativeComp;

        extraPackages = epkgs: [ epkgs.vterm epkgs.melpaPackages.telega ];

        # overrides = self: super: rec {
        #   telega = pkgs.emacsPackages.telega.overrideAttrs (old: {
        #     buildInputs = old.buildInputs ++ [ tdlib ];
        #   });
        # };
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
        lib.mkForce [ "sway-session.target" ];
    })
  ]);
}

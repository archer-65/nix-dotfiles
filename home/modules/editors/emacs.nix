_:
{ config, lib, pkgs, inputs, ... }:

with lib;
let
  cfg = config.user-modules.editors.emacs;
  configDir = config.dotfiles.configDir;
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
        extraPackages = epkgs:
          [
            epkgs.vterm
            # epkgs.telega
          ];
      };

      home.packages = [ pkgs.tdlib ];

      home.file.".emacs.d" = {
        source = "${configDir}/emacs";
        recursive = true;
      };
    }

    (mkIf cfg.daemon.enable {
      services.emacs = {
        enable = true;
        client.enable = true;
        defaultEditor = true;
      };

      #systemd.user.services.emacs = lib.mkForce {};
    })
  ]);
}

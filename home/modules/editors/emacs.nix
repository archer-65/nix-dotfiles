{ config, lib, pkgs, inputs, ... }:

with lib;
let cfg = config.user-modules.editors.emacs;
    configDir = config.dotfiles.configDir;
in {
  options.user-modules.editors.emacs = {
    enable = config.lib.added.mkBoolOpt false;
    daemon.enable = config.lib.added.mkBoolOpt false;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      # nixpkgs.overlays = [ inputs.emacs-overlay.overlay ];
      programs.emacs = {
        enable = true;
        package = pkgs.emacsPgtkNativeComp;
        #extraPackages = epkgs: [epkgs.vterm];
      };
    }

    (mkIf cfg.daemon.enable {
      services.emacs = {
        enable = true;
        client.enable = true;
        defaultEditor = true;
      };
    })    
  ]);
}
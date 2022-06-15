_:
{ options, config, lib, pkgs, ... }:

with lib;
let 
  cfg = config.user-modules.desktop.apps.telegram;
  cfgEmacs = config.user-modules.editors.editors.emacs;
in {
  options.user-modules.desktop.apps.telegram = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
    
    telega.enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable (mkMerge [
    { home.packages = [ tdesktop ]; }

    (mkIf (cfg.telega.enable && cfgEmacs.enable) {
      home.packages = [ tdlib ];
    })
  ]);
}

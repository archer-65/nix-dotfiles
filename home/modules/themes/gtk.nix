_: { options, config, lib, pkgs, ... }:

with lib;
let cfg = config.user-modules.theme.gtk;
in {
  options.user-modules.theme.gtk = with types; {
    active = mkOption {
      type = nullOr str;
      default = null;
      apply = v: let theme = builtins.getEnv "THEME"; in
                 if theme != "" then theme else v;
      description = ''
        Name of the theme to enable. Can be overridden by the THEME environment
        variable. Themes can also be hot-swapped with 'hey theme $THEME'.
      '';
    };
    
    font = {
      name = mkOption { 
        type = str;
        default = "Sans";
      };

      size = mkOption { 
        type = int;
        default = 12;
      };
    };
  };
}
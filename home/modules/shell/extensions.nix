_:
{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.user-modules.shell.extensions;
in {
  options.user-modules.shell.extensions = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    programs.bat.enable = true;
    
    programs.exa = {
      enable = true;
      enableAliases = true;
    };

    programs.fzf.enable = true;

    home.packages = with pkgs; [ bat-extras.batman fd ripgrep ];
  };
}

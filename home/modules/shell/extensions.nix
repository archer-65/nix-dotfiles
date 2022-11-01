{ options, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.user-modules.shell.extensions;
  cfgBash = config.user-modules.shell.bash;
in {
  options.user-modules.shell.extensions = {
    enable = mkEnableOption "shell useful commands (e.g. bat, exa) ";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      programs.bat.enable = true;

      programs.exa = {
        enable = true;
        enableAliases = true;
      };

      programs.fzf.enable = true;

      home.packages = with pkgs; [ bat-extras.batman fd ripgrep ];
    }

    # Useful aliases for our shells
    (mkIf cfgBash.enable {
      programs.bash.shellAliases = {
        cat = "${pkgs.bat}/bin/bat";
      };
    })
  ]);
}

{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.shell.extensions;
  cfgBash = config.mario.modules.shell.bash;
in {
  options.mario.modules.shell.extensions = {
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

      programs.nix-index = {
        enable = true;
        enableBashIntegration = true;
      };

      home.packages = with pkgs; [bat-extras.batman fd ripgrep];
    }

    # Useful aliases for our shells
    (mkIf cfgBash.enable {
      programs.bash.shellAliases = {
        cat = "${pkgs.bat}/bin/bat";
      };
    })
  ]);
}

{
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.shell.direnv;
in {
  options.mario.modules.shell.direnv = {
    enable = mkEnableOption "direnv and extensions";
  };

  config = mkIf cfg.enable {
    programs.direnv = {
      enable = true;
      enableBashIntegration = true;
      enableZshIntegration = true;
    };

    programs.direnv.nix-direnv.enable = true; # better than lorri?
    # services.lorri.enable = true;
  };
}

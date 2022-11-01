{
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.home.modules.shell.direnv;
in {
  options.home.modules.shell.direnv = {
    enable = mkEnableOption "direnv and extensions";
  };

  config = mkIf cfg.enable {
    programs.direnv = {
      enable = true;
      enableBashIntegration = true;
    };

    programs.direnv.nix-direnv.enable = true; # better than lorri?
    # services.lorri.enable = true;
  };
}

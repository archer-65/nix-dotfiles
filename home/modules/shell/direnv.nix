{ options, config, lib, ... }:

with lib;
let cfg = config.user-modules.shell.direnv;
in {
  options.user-modules.shell.direnv = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    programs.direnv = {
      enable = true;
      enableBashIntegration = true;
    };

    # Better than lorri?
    programs.direnv.nix-direnv.enable = true;
    # services.lorri.enable = true;
  };
}

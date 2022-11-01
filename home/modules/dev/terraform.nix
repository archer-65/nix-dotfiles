_:
{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.user-modules.dev.terraform;
in {
  options.user-modules.dev.terraform = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config =
    mkIf cfg.enable { home.packages = with pkgs; [ terraform terraform-ls ]; };
}

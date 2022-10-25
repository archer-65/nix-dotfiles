_:
{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.user-modules.dev.rust;
in {
  options.user-modules.dev.rust = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      rustup
      rust-analyzer
    ];
  };
}

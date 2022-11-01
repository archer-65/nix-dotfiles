{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.user-modules.dev.tex;
in {
  options.user-modules.dev.tex = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs.texlive; [ combined.scheme-full ];
  };
}

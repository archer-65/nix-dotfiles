_:
{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.user-modules.dev.nix;
in {
  options.user-modules.dev.nix = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ statix manix deadnix nixfmt rnix-lsp ];
  };
}

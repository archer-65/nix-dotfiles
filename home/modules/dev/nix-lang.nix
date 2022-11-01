{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.user-modules.dev.nix;
in {
  options.user-modules.dev.nix = {
    enable = mkEnableOption "nix language extra tools and language server";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      statix
      deadnix
      alejandra
      rnix-lsp
      # nil
    ];
  };
}

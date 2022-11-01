{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.user-modules.dev.rust;
in {
  options.user-modules.dev.rust = {
    enable = mkEnableOption "rust language support and language server";
  };

  config =
    mkIf cfg.enable {home.packages = with pkgs; [rustup rust-analyzer];};
}

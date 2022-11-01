{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.home.modules.dev.rust;
in {
  options.home.modules.dev.rust = {
    enable = mkEnableOption "rust language support and language server";
  };

  config =
    mkIf cfg.enable {home.packages = with pkgs; [rustup rust-analyzer];};
}

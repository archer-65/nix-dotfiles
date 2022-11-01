{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.user-modules.desktop.gaming.emulators;
in {
  options.user-modules.desktop.gaming.emulators = {
    switch.enable = mkEnableOption "switch emulator";
  };

  config = {
    home.packages = with pkgs; [(mkIf cfg.switch.enable yuzu-mainline)];
  };
}

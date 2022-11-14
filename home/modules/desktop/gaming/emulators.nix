{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.desktop.gaming.emulators;
in {
  options.mario.modules.desktop.gaming.emulators = {
    switch.enable = mkEnableOption "switch emulator";
  };

  config = {
    home.packages = with pkgs; [(mkIf cfg.switch.enable yuzu-mainline)];
  };
}

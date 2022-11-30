{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.gaming.emulators;
in {
  options.mario.modules.gaming.emulators = {
    switch.enable = mkEnableOption "switch emulator";
  };

  config = {
    home.packages = with pkgs; [(mkIf cfg.switch.enable yuzu-mainline)];
  };
}

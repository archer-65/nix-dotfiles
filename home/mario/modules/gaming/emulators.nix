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
    # Keep this as reference, as it is now considered illegal :/
    # switch.enable = mkEnableOption "switch emulator";
  };

  config = {
    # As stated above
    # home.packages = with pkgs; [(mkIf cfg.switch.enable yuzu-mainline)];
  };
}

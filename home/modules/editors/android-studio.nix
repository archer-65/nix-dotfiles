{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.home.modules.editors.android;
in {
  options.home.modules.editors.android = {
    enable = mkEnableOption "android IDE and screen mirroring";
  };

  config =
    mkIf cfg.enable {home.packages = with pkgs; [android-studio scrcpy];};
}

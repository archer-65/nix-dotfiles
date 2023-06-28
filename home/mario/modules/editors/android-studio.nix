{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.editors.android;
in {
  options.mario.modules.editors.android = {
    enable = mkEnableOption "android IDE and screen mirroring";
  };

  config =
    mkIf cfg.enable {home.packages = with pkgs; [android-studio scrcpy];};
}

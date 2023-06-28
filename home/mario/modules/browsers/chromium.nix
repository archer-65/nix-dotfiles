{
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.browsers.chromium;
in {
  options.mario.modules.browsers.chromium = {
    enable = mkEnableOption "chromium";
  };

  config = mkIf cfg.enable {
    programs.chromium = {
      enable = true;
      commandLineArgs = ["--ozone-platform-hint=auto"];
    };
  };
}

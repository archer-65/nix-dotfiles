{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.home.modules.desktop.browsers.chromium;
in {
  options.home.modules.desktop.browsers.chromium = {
    enable = mkEnableOption "chromium";
  };

  config = mkIf cfg.enable {
    programs.chromium = {
      enable = true;
      commandLineArgs = ["--ozone-platform-hint=auto --force-dark-mode --enable-features=WebUIDarkMode"];
    };
  };
}

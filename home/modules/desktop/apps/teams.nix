{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.desktop.apps.teams;
in {
  options.mario.modules.desktop.apps.teams = {
    enable = mkEnableOption "teams";
  };

  config = mkIf cfg.enable {
    mario.modules.desktop.browsers.chromium.enable = true;

    home.packages = with pkgs; let
      teams-chromium = makeDesktopItem {
        name = "Teams";
        desktopName = "Teams";
        genericName = "Microsoft Teams";
        exec = ''
          ${config.programs.chromium.package}/bin/chromium --ozone-platform-hint=auto --force-dark-mode --enable-features=WebUIDarkMode --app="https://teams.live.com"'';
        icon = "teams";
        categories = ["Network" "InstantMessaging"];
        mimeTypes = ["x-scheme-handler/teams"];
      };
    in [teams-chromium];
  };
}

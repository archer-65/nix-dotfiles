{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.user-modules.desktop.apps.teams;
in {
  options.user-modules.desktop.apps.teams = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    user-modules.desktop.browsers.chromium.enable = true;

    home.packages = with pkgs;
      let
        teams-chromium = makeDesktopItem {
          name = "Teams";
          desktopName = "Teams";
          genericName = "Microsoft Teams";
          exec = ''
            ${config.programs.chromium.package}/bin/chromium --ozone-platform-hint=auto --app="https://teams.live.com"'';
          icon = "teams";
          categories = [ "Network" "InstantMessaging" ];
          mimeTypes = [ "x-scheme-handler/teams" ];
        };
      in [ teams-chromium ];
  };
}

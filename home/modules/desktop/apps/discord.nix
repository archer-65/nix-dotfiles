{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.home.modules.desktop.apps.discord;
  cfgWayland = config.home.modules.desktop.wayland;
in {
  options.home.modules.desktop.apps.discord = {
    enable = mkEnableOption "discord";
  };

  config = mkIf cfg.enable (mkMerge [
    (mkIf cfgWayland.enable {
      home.modules.desktop.browsers.chromium.enable = true;

      home.packages = with pkgs; let
        discord-chromium = makeDesktopItem {
          name = "Discord";
          desktopName = "Discord";
          genericName = "All-in-one cross-platform voice and text chat for gamers";
          exec = ''
            ${config.programs.chromium.package}/bin/chromium --ozone-platform-hint=auto --app="https://discord.com/channels/@me"'';
          icon = "discord";
          type = "Application";
          categories = ["Network" "InstantMessaging"];
          terminal = false;
          mimeTypes = ["x-scheme-handler/discord"];
        };
      in [discord-chromium];
    })

    (mkIf (!cfgWayland.enable) {home.packages = with pkgs; [discord];})
  ]);
}

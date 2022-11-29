{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.desktop.apps.discord;

  discord-chromium = pkgs.makeDesktopItem {
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
in {
  options.mario.modules.desktop.apps.discord = {
    enable = mkEnableOption "discord";
  };

  config = mkIf cfg.enable {
    mario.modules.desktop.browsers.chromium.enable = true;

    home.packages = with pkgs; [
      discord-chromium

      # Enable again when WebRTC is hooked in Pipewire
      # (discord-canary.override {
      #   nss = pkgs.nss_latest;
      #   withOpenASAR = true;
      # })
    ];
  };
}

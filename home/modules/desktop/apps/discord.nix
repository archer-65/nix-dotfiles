{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.mario.modules.desktop.apps.discord;

  discord-chromium = pkgs.makeDesktopItem {
    name = "Discord";
    desktopName = "Discord";
    genericName = "All-in-one cross-platform voice and text chat for gamers";
    exec = ''
      ${config.programs.chromium.package}/bin/chromium --ozone-platform-hint=auto  --force-dark-mode --enable-features=WebUIDarkMode --app="https://discord.com/channels/@me"'';
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

    programs.webcord = {
      enable = true;
      themes = let
        catppuccin = pkgs.fetchFromGitHub {
          owner = "catppuccin";
          repo = "discord";
          rev = "159aac939d8c18da2e184c6581f5e13896e11697";
          sha256 = "sha256-cWpog52Ft4hqGh8sMWhiLUQp/XXipOPnSTG6LwUAGGA=";
        };
      in {
        CatpuccinMocha = "${catppuccin}/themes/mocha.theme.css";
      };
    };

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

{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.browsers.firefox;
  cfgWayland = config.mario.modules.wayland;
in {
  options.mario.modules.browsers.firefox = {
    enable = mkEnableOption "firefox with a decent configuration";
  };

  config = mkIf cfg.enable {
    programs.firefox = {
      enable = true;
      package =
        if cfgWayland.enable
        then pkgs.firefox-wayland
        else pkgs.firefox;

      profiles.default = {
        name = "Default";
        settings = {
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "browser.uidensity" = 0;
          "svg.context-properties.content.enabled" = true;

          "network.trr.mode" = 2;
          "network.trr.uri" = "https://dns.quad9.net/dns-query";
        };
      };
    };


  };
}

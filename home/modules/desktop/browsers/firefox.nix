{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.desktop.browsers.firefox;
  cfgWayland = config.mario.modules.desktop.wayland;
in {
  options.mario.modules.desktop.browsers.firefox = {
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
          # For Firefox GNOME theme:
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "browser.uidensity" = 0;
          "svg.context-properties.content.enabled" = true;
        };
        # userChrome = ''
        #   @import "firefox-gnome-theme/userChrome.css";
        # '';
        # userContent = ''
        #   @import "firefox-gnome-theme/userContent.css";
        # '';
      };
    };

    home.file."firefox-gnome-theme" = {
      target = ".mozilla/firefox/default/chrome/firefox-gnome-theme";
      source = pkgs.fetchFromGitHub {
        owner = "rafaelmardojai";
        repo = "firefox-gnome-theme";
        rev = "v105";
        sha256 = "sha256-CXRKJ+xsv+/jN5DIpLFGvMH0XgVMbJjn1DkFIsZ4d4k=";
      };
    };
  };
}

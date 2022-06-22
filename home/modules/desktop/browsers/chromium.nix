_:
{ options, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.user-modules.desktop.browsers.chromium;

  custom-chromium = with pkgs; [
    (chromium.override {
      commandLineArgs = "--ozone-platform-hint=auto --enable-features=WebRTCPipeWireCapturer --enable-usermedia-screen-capturing";
    })
  ];
in {
  options.user-modules.desktop.browsers.chromium = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    programs.chromium = {
      enable = true;
      commandLineArgs = ["--ozone-platform-hint=auto" "--enable-features=WebRTCPipeWireCapturer" "--enable-usermedia-screen-capturing"];
    };
  };
}

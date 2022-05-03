{ config, lib, pkgs, ... }:

let
  # Overriding nerd fonts (if you don't, all nerd fonts will be installed.)
  nerdFonts = pkgs.nerdfonts.override {
    fonts = [
      "FiraCode"
      "JetBrainsMono"
      "VictorMono"
      "Iosevka"
    ];
  };
in {
  # System fonts
  fonts.fonts = with pkgs; [
    source-code-pro
    font-awesome
    corefonts
    nerdFonts
    source-han-sans
  ];
}
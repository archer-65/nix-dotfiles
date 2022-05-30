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
    nerdFonts
    corefonts
    source-code-pro
    source-han-sans
    font-awesome
    noto-fonts-emoji
  ];
}

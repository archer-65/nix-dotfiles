_:
{ config, lib, pkgs, ... }:

with lib;

let
  cfgDependency = config.modules.desktop.xorg;

  # Overriding nerd fonts (if you don't, all nerd fonts will be installed.)
  nerdFonts = pkgs.nerdfonts.override {
    fonts = [ "FiraCode" "JetBrainsMono" "VictorMono" "Iosevka" ];
  };
in {

  config = mkIf cfgDependency.enable {
    # System fonts
    fonts.fonts = with pkgs; [
      nerdFonts
      corefonts
      source-code-pro
      source-han-sans
      font-awesome
      noto-fonts-emoji
    ];
  };
}

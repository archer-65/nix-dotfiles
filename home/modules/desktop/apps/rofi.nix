_:
{ pkgs, config, ... }:
let
  # scripts = pkgs.callPackage ./scripts { inherit config pkgs; };
  scripts = with pkgs.scripts.rofi; [
    usedcpu
    usedram
    powermenu
    launcher
    greenclip
    emoji
  ];
  # launchers = pkgs.callPackage ./launchers { inherit config pkgs; };
in {
  programs.rofi = {
    enable = true;

    plugins = with pkgs;
      [
        rofi-emoji
        # rofi-rbw
      ];
  };

  xdg.configFile."rofi/colors" = {
    source = ../../../../config/rofi/colors;
    recursive = true;
  };

  xdg.configFile."rofi/themes" = {
    source = ../../../../config/rofi/themes;
    recursive = true;
  };

  home.packages = scripts;
}

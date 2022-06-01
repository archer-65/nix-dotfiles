{ pkgs, config, ... }:
let
  scripts = pkgs.callPackage ./scripts { inherit config pkgs; };
  launchers = pkgs.callPackage ./launchers { inherit config pkgs; };
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
    source = ./colors;
    recursive = true;
  };

  xdg.configFile."rofi/themes" = {
    source = ./themes;
    recursive = true;
  };

  home.packages = scripts ++ launchers;
}

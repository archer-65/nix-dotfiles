{ pkgs, config, ... }:
let
  scripts = pkgs.callPackage ./rofi/scripts { inherit config pkgs; };
  launchers = pkgs.callPackage ./rofi/launchers { inherit config pkgs; };
in {
  programs.rofi = {
    enable = true;

    plugins = with pkgs; [
      rofi-emoji 
      # rofi-rbw
    ];
  };

  xdg.configFile."rofi" = {
    source = ./rofi;
    recursive = true;
  };

  home.packages = scripts ++ launchers; 
}
{ pkgs, ... }:

{
  home.packages = with pkgs; [
    wl-clipboard
    wtype
    #azote
    #swhkd
    grim
    slurp
    sway-contrib.grimshot
  ];
}

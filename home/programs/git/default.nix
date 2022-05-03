{ config, pkgs, ... }:

{
  programs.git = {
    enable = true;
    userEmail = "mariogt2009@live.it";
    userName = "archer-65";
  };
}
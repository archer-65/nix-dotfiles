{ config, pkgs, ... }:

let
  emoji = pkgs.callPackage ./emoji.nix { inherit pkgs; };
  clipboard = pkgs.callPackage ./greenclip.nix { inherit pkgs; };
  launcher = pkgs.callPackage ./launcher.nix { inherit pkgs; };
  powermenu = pkgs.callPackage ./powermenu.nix { inherit pkgs; };
in [ emoji clipboard launcher powermenu ]

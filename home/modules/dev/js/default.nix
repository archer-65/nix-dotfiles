{ options, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.user-modules.dev.js;
  #node-pkgs = pkgs.callPackage ./node-pkgs { };
in {
  options.user-modules.dev.js = {
    enable = mkEnableOption "javascript language tools";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      # Base
      nodejs

      # NPM extra packages
      node2nix
      #node-pkgs."pkgs"

      # Language server
      nodePackages.typescript-language-server
    ];

    # Needed to quickly install borked modules (e.g. amplify, *coff*) in home
    # Not recommended, though
    home.sessionVariables = {
      PATH = "$HOME/.mutable_node_modules/bin:$PATH";
      NODE_PATH = "$HOME/.mutable_node_modules/lib/node_modules";
    };
  };
}

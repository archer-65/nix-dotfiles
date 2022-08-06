_:
{ options, config, lib, pkgs, ... }:

with lib;
let
  cfg = config.user-modules.dev.js;
  #node-pkgs = pkgs.callPackage ./node-pkgs { };
in {
  options.user-modules.dev.js = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      # Base
      nodejs
      node2nix

      # NPM
      #node-pkgs."pkgs"
    ];

    home.sessionVariables = {
      PATH = "$HOME/.mutable_node_modules/bin:$PATH";
      NODE_PATH = "$HOME/.mutable_node_modules/lib/node_modules";
    };
  };
}

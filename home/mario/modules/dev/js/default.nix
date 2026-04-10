{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.dev.js;
in {
  options.mario.modules.dev.js = {
    enable = mkEnableOption "javascript language tools";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      nodejs
      typescript-language-server
    ];

    # Needed to quickly install borked modules (e.g. amplify, *coff*) in home
    # Not recommended, though
    home.sessionVariables = {
      PATH = "$HOME/.mutable_node_modules/bin:$PATH";
      NODE_PATH = "$HOME/.mutable_node_modules/lib/node_modules";
    };
  };
}

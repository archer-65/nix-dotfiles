{
  pkgs,
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.shell.zsh;
in {
  options.mario.modules.shell.zsh = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    programs.zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableCompletion = true;
      enableSyntaxHighlighting = true;
      shellAliases = {};
    };
  };
}

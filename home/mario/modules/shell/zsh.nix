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
      initExtra = ''
        zstyle ":completion:*" menu select
        zmodload zsh/complist
               compinit
               _comp_options+=(globdots)

               bindkey '^[[1;5D' backward-word
               bindkey '^[[1;5C' forward-word
      '';
    };
  };
}

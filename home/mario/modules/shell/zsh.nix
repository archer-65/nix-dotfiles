{
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
      enableCompletion = true;
      autosuggestion.enable = true;
      syntaxHighlighting.enable = true;
      shellAliases = {};
      initExtra = ''
        # Case insensitive tab completion
        zstyle ':completion:*' completer _complete _ignored _approximate
        zstyle ':completion:*' list-colors '\'
        zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
        zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
        zstyle ':completion:*' menu select
        zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
        zstyle ':completion:*' verbose true
        _comp_options+=(globdots)

        zmodload zsh/complist

        # Keybindings
        bindkey '^[[1;5D' backward-word
        bindkey '^[[1;5C' forward-word
        bindkey '^[[3;5~' kill-word

        bindkey ' ' magic-space  # [Space] - Don't do history expansion

        # Edit current command line in $EDITOR
        autoload -U edit-command-line
        zle -N edit-command-line
        bindkey '^X^e' edit-command-line
      '';
    };
  };
}

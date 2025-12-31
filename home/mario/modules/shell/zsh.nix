{
  pkgs,
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.shell.zsh;

  initContent = let
    # FIXME: Beautiful, but flashing on selection!
    # Old issue: https://github.com/Aloxaf/fzf-tab/issues/172
    early = lib.mkBefore ''
      # source  ${pkgs.zsh-fzf-tab}/share/fzf-tab/fzf-tab.plugin.zsh
    '';

    general = ''
      # Keybindings
      bindkey "^[[3~" delete-char
      bindkey '^[[1;5D' backward-word
      bindkey '^[[1;5C' forward-word
      bindkey '^[[3;5~' kill-word

      bindkey ' ' magic-space  # [Space] - Don't do history expansion

      # FIXME: https://github.com/nix-community/home-manager/issues/2991
      if [ -r "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" ]; then
        __HM_SESS_VARS_SOURCED= source "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
      fi

      [ -f /opt/homebrew/bin/brew ] && eval "$(/opt/homebrew/bin/brew shellenv)"
    '';

    last = lib.mkAfter ''
      # Source private configuration if exists
      [[ -e "$HOME/.config/zsh/private.zsh" ]] && source "$HOME/.config/zsh/private.zsh"
    '';
  in
    lib.mkMerge [early general last];
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

      history = {
        expireDuplicatesFirst = true;
        extended = true;
        ignoreDups = true;
        ignorePatterns = [];
        ignoreSpace = false;
        save = 10000;
        share = true;
        size = 10000;
      };

      shellAliases = {};

      completionInit = ''
        zmodload zsh/zle
        zmodload zsh/zpty
        zmodload zsh/complist

        _comp_options+=(globdots)

        # Edit current command line in $EDITOR
        autoload -U edit-command-line
        zle -N edit-command-line
        bindkey '^X^e' edit-command-line

        # Case insensitive tab completion ('C-x a' to expand_alias)
        zstyle ':completion:*' completer _extensions _complete _ignored _approximate

        # Use cache
        zstyle ':completion:*' use-cache on
        zstyle ':completion:*' cache-path "$XDG_CACHE_HOME/zsh/.zcompcache"

        # Complete the alias
        zstyle ':completion:*' complete true

        # Autocomplete options
        zstyle ':completion:*' complete-options true

        # Completion matching control
        # WIP: Try new matcher
        # zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
        zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
        zstyle ':completion:*' keep-prefix true

        # Group matches and describe
        zstyle ':completion:*' menu select
        zstyle ':completion:*' list-grouped false
        zstyle ':completion:*' list-separator '''
        zstyle ':completion:*' group-name '''
        zstyle ':completion:*' verbose true
        zstyle ':completion:*:matches' group 'yes'
        zstyle ':completion:*:warnings' format '%F{red}%B-- No match for: %d --%b%f'
        zstyle ':completion:*:messages' format '%d'
        zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'
        zstyle ':completion:*:descriptions' format '[%d]'

        # Colors
        # zstyle ':completion:*' list-colors '\'
        zstyle ':completion:*' list-colors ''${(s.:.)LS_COLORS}

        # Prompt
        zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
        zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s

        # Directories
        zstyle ':completion:*:*:cd:*' tag-order local-directories directory-stack path-directories
        zstyle ':completion:*:*:cd:*:directory-stack' menu yes select
        zstyle ':completion:*:-tilde-:*' group-order 'named-directories' 'path-directories' 'users' 'expand'
        zstyle ':completion:*:*:-command-:*:*' group-order aliases builtins functions commands
        zstyle ':completion:*' special-dirs true
        zstyle ':completion:*' squeeze-slashes true

        # Sort
        zstyle ':completion:*' sort false
        zstyle ":completion:*:git-checkout:*" sort false
        zstyle ':completion:*' file-list all
        zstyle ':completion:*' file-sort modification
        zstyle ':completion:*:eza' sort false
        zstyle ':completion:complete:*:options' sort false
        zstyle ':completion:files' sort false

        # Plugin: 'fzf-tab'
        zstyle ':fzf-tab:complete:kill:argument-rest' fzf-preview 'ps --pid=$word -o cmd --no-headers -w -w'
        zstyle ':fzf-tab:complete:kill:argument-rest' fzf-flags '--preview-window=down:3:wrap'
        zstyle ':fzf-tab:*' fzf-command fzf
        zstyle ':fzf-tab:*' fzf-pad 4
        zstyle ':fzf-tab:*' fzf-min-height 100
        zstyle ':fzf-tab:*' switch-group ',' '.'
      '';

      inherit initContent;

      # TODO: fast-syntax-highlighting?
      plugins = [
        {
          name = "zsh-autopair";
          src = pkgs.zsh-autopair;
          file = "share/zsh/zsh-autopair/autopair.zsh";
        }
        {
          name = "zsh-you-should-use";
          src = pkgs.zsh-you-should-use;
          file = "share/zsh/plugins/you-should-use/you-should-use.plugin.zsh";
        }
        {
          name = "zsh-nix-shell";
          src = pkgs.zsh-nix-shell;
          file = "share/zsh-nix-shell/nix-shell.plugin.zsh";
        }
      ];
    };
  };
}

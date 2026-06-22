{
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.shell.zsh;

  initContent = let
    general = ''
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
        autoload -U compinit
        zmodload zsh/complist

        _comp_options+=(globdots)
        zcompdump="$XDG_DATA_HOME"/zsh/.zcompdump-"$ZSH_VERSION"-"$(date -I)"
        compinit -d "$zcompdump"

        # Recompile zcompdump if it exists and is newer than zcompdump.zwc
        # compdumps are marked with the current date in yyyy-mm-dd format
        # which means this is likely to recompile daily
        # also see: <https://htr3n.github.io/2018/07/faster-zsh/>
        if [[ -s "$zcompdump" && (! -s "$zcompdump".zwc || "$zcompdump" -nt "$zcompdump".zwc) ]]; then
          zcompile "$zcompdump"
        fi

        # Load bash completion functions.
        autoload -U +X bashcompinit && bashcompinit

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
        zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*' 'm:{[:lower:][:upper:]}={[:upper:][:lower:]} l:|=* r:|=*'
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
      '';

      initContent = ''
        zstyle ':fzf-tab:complete:cd:*'       fzf-preview 'eza -1 --color=always $realpath'
        zstyle ':fzf-tab:*'                   switch-group ',' '.'
        zstyle ':fzf-tab:complete:cd:*'       popup-pad 20 0

        zstyle ':fzf-tab:complete:kill:argument-rest' fzf-preview 'ps --pid=$word -o cmd --no-headers -w -w'
        zstyle ':fzf-tab:complete:kill:argument-rest' fzf-flags '--preview-window=down:3:wrap'

        # Keybindings
        bindkey "^[[3~" delete-char
        bindkey '^[[1;5D' backward-word
        bindkey '^[[1;5C' forward-word
        bindkey '^[[3;5~' kill-word

        bindkey ' ' magic-space  # [Space] - Don't do history expansion

        # Interactive line editing in the shell
        zmodload zsh/zle

        # Control interactive processes
        zmodload zsh/zpty
      '';

      # TODO: Try out fast-syntax-highlighting?
      plugins = [
        # Must be before plugins that wrap widgets, such as zsh-autosuggestions or fast-syntax-highlighting
        {
          name = "fzf-tab";
          file = "share/fzf-tab/fzf-tab.plugin.zsh";
          src = pkgs.zsh-fzf-tab;
        }
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
        # TODO: Consider the usage of https://github.com/MercuryTechnologies/nix-your-shell instead?
        {
          name = "zsh-nix-shell";
          src = pkgs.zsh-nix-shell;
          file = "share/zsh-nix-shell/nix-shell.plugin.zsh";
        }
      ];
    };
  };
}

{
  pkgs,
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.shell.tmux;
in {
  options.mario.modules.shell.tmux = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    programs.tmux = {
      enable = true;
      prefix = "C-Space";
      newSession = true;
      baseIndex = 1;
      clock24 = true;
      historyLimit = 8000;
      escapeTime = 20;
      terminal = "tmux-256color";
      sensibleOnTop = false;
      mouse = true;

      extraConfig = ''
        set -ga terminal-overrides ",alacritty*:Tc"

        unbind %
        bind | split-window -h

        unbind '"'
        bind - split-window -v

        unbind R
        bind R source-file ~/.config/tmux/tmux.conf

        bind -r j resize-pane -D 5
        bind -r k resize-pane -U 5
        bind -r l resize-pane -R 5
        bind -r h resize-pane -L 5

        bind -r m resize-pane -Z
      '';

      plugins = with pkgs; [
        {
          plugin = tmuxPlugins.catppuccin.overrideAttrs (_: rec {
            version = "2.1.2";
            src = pkgs.fetchFromGitHub {
              owner = "catppuccin";
              repo = "tmux";
              rev = "v${version}";
              sha256 = "sha256-vBYBvZrMGLpMU059a+Z4SEekWdQD0GrDqBQyqfkEHPg=";
            };
          });
          extraConfig = with config.colorScheme.palette; ''
            set -g @catppuccin_flavor "mocha"

            set -g @catppuccin_status_background "none"
            set -g @catppuccin_window_status_style "basic"

            set -g @catppuccin_window_number_position "right"
            set -g @catppuccin_window_current_text "#W"
            set -g @catppuccin_window_current_number_color "#{@thm_green}"

            set -g @catppuccin_status_left_separator "█"
            set -g @catppuccin_status_middle_separator ""
            set -g @catppuccin_status_right_separator "█"

            set -g @catppuccin_directory_text "#{pane_current_path}"

            set -g status-left  ""
            set -g status-right "#{E:@catppuccin_status_application}#{E:@catppuccin_status_session}"
          '';
        }
      ];
    };
  };
}

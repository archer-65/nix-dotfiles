{
  pkgs,
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.shell.tmux;

  theme = with config.colorScheme.palette; ''
    # --> Catppuccin (Dynamic)
    thm_bg=\"#${strings.toLower base00}\"
    thm_fg=\"#${strings.toLower base05}\"
    thm_cyan=\"#${strings.toLower base0C}\"
    thm_black=\"#${strings.toLower base01}\"
    thm_gray=\"#${strings.toLower base01}\"
    thm_magenta=\"#${strings.toLower base0E}\"
    thm_pink=\"#${strings.toLower base0E}\"
    thm_red=\"#${strings.toLower base08}\"
    thm_green=\"#${strings.toLower base0B}\"
    thm_yellow=\"#${strings.toLower base09}\"
    thm_blue=\"#${strings.toLower base0D}\"
    thm_orange=\"#${strings.toLower base0A}\"
    thm_black4=\"#${strings.toLower base00}\"
  '';
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
          plugin = tmuxPlugins.catppuccin.overrideAttrs (_: {
            version = "unstable-2024-03-24";
            src = pkgs.fetchFromGitHub {
              owner = "catppuccin";
              repo = "tmux";
              rev = "e2d345648cb7e56302ee82fec6cafbbd8db23129";
              hash = "sha256-WCJfDjYUNWqkDyCgYKADDS0SeEpfMnZ57bCsp9steh8=";
            };
            postInstall = ''
              echo "${theme}" > $target/catppuccin-dynamic.tmuxtheme
            '';
          });
          extraConfig = with config.colorScheme.palette; ''
            set -g @catppuccin_flavour "dynamic"

            set -g @catppuccin_status_background "default"

            set -g @catppuccin_status_left_separator "█"
            set -g @catppuccin_status_right_separator "█"
            set -g @catppuccin_window_middle_separator " █"
            set -g @catppuccin_window_number_position "right"

            set -g @catppuccin_window_default_fill "number"
            set -g @catppuccin_window_default_text "#W"

            set -g @catppuccin_window_current_fill "number"
            set -g @catppuccin_window_current_text "#W"
            set -g @catppuccin_window_current_background "#${strings.toLower base02}"

            set -g @catppuccin_status_modules_right "directory session"

            set -g @catppuccin_weather_icon " "
            set -g @catppuccin_uptime_icon "󰔟 "
            set -g @catppuccin_load_icon "󰊚 "
            set -g @catppuccin_battery_icon "#{battery_icon}"
            set -g @catppuccin_host_icon "󰒋 "
            set -g @catppuccin_cpu_icon " "
            set -g @catppuccin_application_icon " "
            set -g @catppuccin_clima_icon " "
            set -g @catppuccin_directory_icon " "
            set -g @catppuccin_date_time_icon "󰃰 "
            set -g @catppuccin_user_icon " "
            set -g @catppuccin_session_icon " "
          '';
        }
      ];
    };
  };
}

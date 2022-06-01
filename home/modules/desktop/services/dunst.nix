_:
{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.user-modules.desktop.services.dunst;
in {
  options.user-modules.desktop.services.dunst = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.libnotify ];

    services.dunst = {
      enable = true;

      settings = {
        global = {
          # Behavior
          follow = "keyboard";
          fullscreen = "pushback";

          # Geometry
          width = "(300, 500)";
          height = "300";
          origin = "top-right";
          offset = "5x40";

          notification_limit = 3;

          # Progress bar
          progress_bar = true;
          progress_bar_height = 15;
          progress_bar_frame_width = 3;
          progress_bar_min_width = 300;
          progress_bar_max_width = 300;

          # Appearance
          indicate_hidden = true;
          transparency = 0;

          separator_height = 1;

          padding = 10;
          horizontal_padding = 10;
          text_icon_padding = 15;

          corner_radius = 0;
          frame_width = 2;
          frame_color = "#282C34";
          separator_color = "frame";

          sort = "yes";
          idle_threshold = 60;

          # Text    
          font = "Fira Code 13, Font Awesome 13";
          line_height = 0;
          markup = "full";

          format = "<b>%s</b>\\n%b";
          alignment = "left";
          vertical_alignment = "center";
          show_age_threshold = 60;
          ellipsize = "middle";
          ignore_newline = "no";
          stack_duplicates = true;
          hide_duplicate_count = true;
          show_indicators = false;

          # Icons
          icon_position = "left";
          min_icon_size = 0;
          max_icon_size = 64;

          # History
          sticky_history = "yes";
          history_length = 50;

          # Misc
          title = "Dunst";
          class = "Dunst";

          dmenu = "${pkgs.rofi}/bin/rofi -dmenu -p dunst:";
          always_run_script = true;
          ignore_dbusclose = false;

          # Wayland
          force_xwayland = false;
          force_xinerama = false;

          # Mouse
          mouse_left_click = "close_current";
          mouse_middle_click = "do_action, close_current";
          mouse_right_click = "close_all";
        };

        urgency_low = {
          background = "#282C34";
          foreground = "#ABB2BF";
          timeout = 5;
        };

        urgency_normal = {
          background = "#282C34";
          foreground = "#56B6C2";
          timeout = 5;
        };

        urgency_critical = {
          background = "#282C34";
          foreground = "#C678DD";
          timeout = 0;
        };

        experimental.per_monitor_dpi = false;
      };
    };
  };
}

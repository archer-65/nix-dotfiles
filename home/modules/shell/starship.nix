{
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.shell.starship;
in {
  options.mario.modules.shell.starship = {
    enable = mkEnableOption "starship configuration";
  };

  config = mkIf cfg.enable {
    programs.starship = {
      enable = true;
      enableBashIntegration = true;
      settings = {
        character = {
          success_symbol = "[❯](bold green)";
          error_symbol = "[✗](bold red)";
        };

        cmd_duration = {
          format = "wasted time [$duration]($style)";
          style = "bold purple";
        };

        directory = {repo_root_style = "bold blue";};

        memory_usage = {
          disabled = false;
          threshold = -1;
          format = "$symbol [$ram( | $swap)]($style) ";
          symbol = "🧠";
          style = "bold yellow";
        };

        username = {
          show_always = true;
          style_user = "bold blue";
        };
      };
    };
  };
}

{ pkgs, ... }:

{
  programs.bash = {
    enable = true;

    shellAliases = {
      ls = "exa";
      cat = "bat";
      find = "fd";
    };

    # sessionVariables = {
    #   VISUAL = "emacsclient -c -a emacs";
    #   EDITOR = "emacsclient -t";    
    # };

    initExtra = ''
      ${pkgs.pfetch}/bin/pfetch
    '';
  };

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

      directory = {
        repo_root_style = "bold blue";
      };

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
}
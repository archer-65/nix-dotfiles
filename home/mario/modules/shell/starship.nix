{
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
      enableZshIntegration = true;
      settings = {
        format = let
          git = "$git_branch$git_commit$git_state$git_status";
        in ''
          $username[@](purple)$hostname $directory (${git}) ($cmd_duration) $fill ($nix_shell)
          $character
        '';

        username = {
          format = "[$user]($style)";
          show_always = true;
          style_user = "bold cyan";
        };

        hostname = {
          format = "[$hostname]($style)";
          ssh_only = false;
          style = "bold green";
        };

        directory = {
          format = "[$path]($style)([$read_only]($read_only_style))";
          style = "bold yellow";
        };

        cmd_duration = {
          format = "took [$duration]($style)";
          style = "bold yellow";
        };

        nix_shell = {
          format = "[($name \\(develop\\) <- )$symbol]($style)";
          impure_msg = "";
          symbol = " ";
          style = "bold blue";
        };

        character = {
          error_symbol = "[λ](bold red)";
          success_symbol = "[λ](bold green)";
        };

        fill = {
          symbol = " ";
          disabled = false;
        };

        aws.symbol = "  ";
        conda.symbol = " ";
        dart.symbol = " ";
        directory.read_only = " ";
        docker_context.symbol = " ";
        elixir.symbol = " ";
        elm.symbol = " ";
        gcloud.symbol = " ";
        git_branch.symbol = " ";
        golang.symbol = " ";
        hg_branch.symbol = " ";
        java.symbol = " ";
        julia.symbol = " ";
        memory_usage.symbol = " ";
        nim.symbol = " ";
        nodejs.symbol = " ";
        package.symbol = " ";
        perl.symbol = " ";
        php.symbol = " ";
        python.symbol = " ";
        ruby.symbol = " ";
        rust.symbol = " ";
        scala.symbol = " ";
        shlvl.symbol = "";
        swift.symbol = "ﯣ ";
        terraform.symbol = "行";
      };
    };
  };
}

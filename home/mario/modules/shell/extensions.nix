{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.shell.extensions;
  cfgBash = config.mario.modules.shell.bash;
  cfgZsh = config.mario.modules.shell.zsh;
in {
  options.mario.modules.shell.extensions = {
    enable = mkEnableOption "shell useful commands (e.g. bat, exa) ";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      programs.bat = {
        enable = true;
        config = {
          theme = "base16";
        };
      };

      programs.exa = {
        enable = true;
        enableAliases = true;
      };

      programs.fzf = {
        enable = true;
        enableBashIntegration = true;
        enableZshIntegration = true;

        tmux.enableShellIntegration = true;
      };

      programs.nix-index = {
        enable = true;
        enableBashIntegration = true;
        enableZshIntegration = true;
      };

      home.packages = with pkgs; [bat-extras.batman fd ripgrep];
    }

    # Useful aliases for our shells
    (mkIf cfgBash.enable {
      programs.bash = {
        bashrcExtra = ''
          export FZF_DEFAULT_OPTS='--color 16'
        '';

        shellAliases = {
          cat = "${pkgs.bat}/bin/bat";
        };
      };
    })

    (mkIf cfgZsh.enable {
      programs.zsh = {
        initExtra = ''
          export FZF_DEFAULT_OPTS='--color 16'
        '';

        shellAliases = {
          cat = "${pkgs.bat}/bin/bat";
        };
      };
    })
  ]);
}

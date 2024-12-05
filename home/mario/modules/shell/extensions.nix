{
  options,
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
with lib; let
  cfg = config.mario.modules.shell.extensions;
  cfgBash = config.mario.modules.shell.bash;
  cfgZsh = config.mario.modules.shell.zsh;
in {
  imports = [inputs.nix-index-database.hmModules.nix-index];

  options.mario.modules.shell.extensions = {
    enable = mkEnableOption "shell useful commands (e.g. bat, eza) ";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs; [fd];
    }

    {
      programs.ripgrep = {
        enable = true;
        arguments = [
          "--max-columns=150"
          "--max-columns-preview"
          "--smart-case"
        ];
      };
    }

    {
      programs.bat = {
        enable = true;
        config = {
          theme = "base16";
        };
        extraPackages = with pkgs.bat-extras; [batman batdiff batgrep];
      };

      home.shellAliases = {
        cat = "${pkgs.bat}/bin/bat";
      };
    }

    {
      programs.eza = {
        enable = true;
        git = true;
        icons = "auto";
      };
    }

    {
      programs.fzf = {
        enable = true;

        defaultOptions = [
          "--ansi"
          "--reverse"
          "--border"
          "--inline-info"
          "--color=16"
        ];

        enableBashIntegration = true;
        enableZshIntegration = true;

        tmux.enableShellIntegration = true;
      };
    }

    # TODO: Refactor out to dedicated module
    {
      programs.nix-index-database.comma.enable = true;

      # `command-not-found` relies on nix-channel.
      # Enable and use `nix-index` instead.
      programs.command-not-found.enable = false;
      programs.nix-index = {
        enable = true;
        enableBashIntegration = true;
        enableZshIntegration = true;
      };
    }
  ]);
}

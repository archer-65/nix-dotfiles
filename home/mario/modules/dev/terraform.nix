{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.dev.terraform;
in {
  options.mario.modules.dev.terraform = {
    enable = mkEnableOption "Terraform package and related tooling";

    enableBashIntegration = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to enable Terraform Bash integration.";
    };

    enableZshIntegration = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to enable Terraform Zsh integration.";
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      terraform
      terraform-ls
      terraform-docs
      tfautomv
      tfmigrate
    ];

    programs.bash.initExtra = lib.mkIf cfg.enableBashIntegration ''
      source ${pkgs.terraform}/share/bash-completion/completions/terraform
    '';

    programs.zsh.initContent = lib.mkIf cfg.enableZshIntegration ''
      autoload -U +X bashcompinit && bashcompinit
      source ${pkgs.terraform}/share/bash-completion/completions/terraform
    '';
  };
}

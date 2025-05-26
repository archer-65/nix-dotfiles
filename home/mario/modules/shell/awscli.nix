{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.mario.modules.shell.awscli;
  iniFormat = pkgs.formats.ini {};
in {
  options.mario.modules.shell.awscli = {
    enable = lib.mkEnableOption "AWS CLI";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.awscli2;
      defaultText = lib.literalExpression "pkgs.awscli2";
      description = "Package providing {command}`aws`.";
    };

    settings = lib.mkOption {
      type = lib.types.submodule {freeformType = iniFormat.type;};
      default = {};
      description = "Configuration written to {file}`$HOME/.aws/config`.";
      example = lib.literalExpression ''
        {
          "default" = {
            region = "eu-west-3";
            output = "json";
          };
        };
      '';
    };

    credentials = lib.mkOption {
      type = lib.types.submodule {freeformType = iniFormat.type;};
      default = {};
      description = ''
        Configuration written to {file}`$HOME/.aws/credentials`.

        For security reasons, never store cleartext passwords here.
        We recommend that you use `credential_process` option to retrieve
        the IAM credentials from your favorite password manager during runtime,
        or use AWS IAM Identity Center to get short-term credentials.

        See https://docs.aws.amazon.com/cli/latest/userguide/cli-chap-authentication.html
      '';
      example = lib.literalExpression ''
        {
          "default" = {
            "credential_process" = "${pkgs.pass}/bin/pass show aws";
          };
        };
      '';
    };

    enableBashIntegration = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to enable AWS CLI's Bash integration.";
    };

    enableZshIntegration = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to enable AWS CLI's Zsh integration.";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [cfg.package pkgs.aws-mfa];

    home.file."${config.home.homeDirectory}/.aws/config" = lib.mkIf (cfg.settings != {}) {
      source =
        iniFormat.generate "aws-config-${config.home.username}" cfg.settings;
    };

    home.file."${config.home.homeDirectory}/.aws/credentials" = lib.mkIf (cfg.credentials != {}) {
      source =
        iniFormat.generate "aws-credentials-${config.home.username}"
        cfg.credentials;
    };

    programs.bash.initExtra = lib.mkIf cfg.enableBashIntegration ''
      source ${cfg.package}/share/bash-completion/completions/aws.bash
    '';

    programs.zsh.initContent = lib.mkIf cfg.enableZshIntegration ''
      source ${cfg.package}/share/zsh/site-functions/_aws
    '';
  };
}

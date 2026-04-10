{
  options,
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.shell.git;
  workDir = config.xdg.userDirs.extraConfig.WORK;
in {
  options.mario.modules.shell.git = {
    enable = mkEnableOption "main user git configuration";
  };

  config = mkIf cfg.enable {
    # TODO: Move these anywhere else, maybe in a `secrets` at the root of the repo
    sops = lib.mkIf (workDir != null) {
      secrets = {
        work_email = {sopsFile = ./secrets/work.yaml;};
        work_username = {sopsFile = ./secrets/work.yaml;};
      };

      templates = {
        work_gitconfig = {
          content = lib.generators.toINI {} {
            user = {
              email = config.sops.placeholder.work_email;
              name = config.sops.placeholder.work_username;
            };
          };
        };
      };
    };

    programs.git = {
      enable = true;
      settings.user.email = "mario.liguori.056@gmail.com";
      settings.user.name = "archer-65";

      signing = {
        key = "BAC570B2172822A3";
        signByDefault = true;
      };

      settings.url = lib.mkIf (workDir != null) {
        "git@github-work:nellotech/" = {
          insteadOf = "git@github.com:nellotech/";
        };
      };

      includes = lib.mkIf (workDir != null) [
        {
          condition = "gitdir:${workDir}/";
          path = config.sops.templates.work_gitconfig.path;
        }
      ];
    };
  };
}

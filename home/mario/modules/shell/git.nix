{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.shell.git;
  cfgWork = config.mario.modules.mixins.work;
in {
  options.mario.modules.shell.git = {
    enable = mkEnableOption "main user git configuration";
  };

  config = mkIf cfg.enable {
    sops.templates = lib.mkIf (cfgWork.enable) {
      work_gitconfig = {
        content = lib.generators.toINI {} {
          user = {
            email = config.sops.placeholder.work_email;
            name = config.sops.placeholder.work_username;
          };
          commit = {
            gpgSign = false;
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
        format = null;
        signByDefault = true;
      };

      settings.url = lib.mkIf (cfgWork.enable) {
        "git@github-work:nellotech/" = {
          insteadOf = "git@github.com:nellotech/";
        };
      };

      includes = lib.mkIf (cfgWork.enable) [
        {
          condition = "gitdir:${config.xdg.userDirs.extraConfig.WORK}/";
          path = config.sops.templates.work_gitconfig.path;
        }
      ];
    };
  };
}

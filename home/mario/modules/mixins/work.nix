{
  options,
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.mixins.work;
in {
  # TODO: May be better to move `mixins` to `features` or other stuff.
  options.mario.modules.mixins.work = {
    enable = mkEnableOption "work mixin functionalities, mainly related to job configs";
  };

  config = mkIf cfg.enable {
    xdg.userDirs.extraConfig.WORK = "${config.home.homeDirectory}/work";

    # TODO: Move these anywhere else, maybe in a `secrets` directory at the root of the repo?
    sops.secrets = {
        work_email = {sopsFile = ./secrets/work.yaml;};
        work_username = {sopsFile = ./secrets/work.yaml;};
    };
  };
}

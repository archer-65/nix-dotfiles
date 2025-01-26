{
  options,
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.shell.git;
  cfgBw = config.mario.modules.credentials.bitwarden;
in {
  options.mario.modules.shell.git = {
    enable = mkEnableOption "main user git configuration";
  };

  config = mkIf cfg.enable {
    programs.git = {
      enable = true;
      userEmail = "mario.liguori.056@gmail.com";
      userName = "archer-65";

      signing = {
        key = "BAC570B2172822A3";
        signByDefault = true;
      };

      extraConfig = {
        credential.helper = lib.mkIf cfgBw.enable "${pkgs.rbw}/bin/git-credential-rbw";
      };

      includes = lib.optionals (builtins.hasAttr "XDG_WORK_DIR" config.xdg.userDirs.extraConfig) [
        {
	  condition = "gitdir:${config.xdg.userDirs.extraConfig.XDG_WORK_DIR}/";
	  contents = {
	    user = {
	      email = "mli@bit4id.com";
	      name = "mli";
	      signingKey = "BAC570B2172822A3";
	    };
	    commit.gpgSign = true;
	  };
	}
      ];
    };
  };
}

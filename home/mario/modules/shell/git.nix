{
  pkgs,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.shell.git;
  cfgWork = config.mario.modules.mixins.work;

  # gitsign-credential-cache binds via Go's os.UserCacheDir(), which on Darwin
  # is always ~/Library/Caches regardless of $XDG_CACHE_HOME.
  gitsignCacheDir =
    if pkgs.stdenv.hostPlatform.isDarwin
    then "${config.home.homeDirectory}/Library/Caches"
    else config.xdg.cacheHome;
  gitsignCacheSocket = "${gitsignCacheDir}/sigstore/gitsign/cache.sock";
in {
  options.mario.modules.shell.git = {
    enable = mkEnableOption "main user git configuration";
  };

  config = mkIf cfg.enable (mkMerge [
    {
      programs.git = {
        enable = true;
        settings.user.email = "mario.liguori.056@gmail.com";
        settings.user.name = "archer-65";

        signing = {
          key = "BAC570B2172822A3";
          format = null;
          signByDefault = true;
        };
      };
    }

    (mkIf cfgWork.enable (mkMerge [
      {
        sops.templates.work_gitconfig = {
          content = lib.generators.toGitINI {
            user = {
              email = config.sops.placeholder.work_email;
              name = config.sops.placeholder.work_username;
            };
            commit.gpgSign = true;
            tag.gpgSign = true;
            gpg = {
              format = "x509";
              x509.program = "${pkgs.gitsign}/bin/gitsign";
            };
            # Entra returns the corporate UPN every time
            gitsign.connectorID = "https://login.microsoftonline.com";
          };
        };

        home.packages = [pkgs.gitsign];

        # Without this, gitsign runs a fresh GitHub OAuth flow on every commit.
        # gitsign-credential-cache (below) caches the short-lived Sigstore cert so
        # only the first commit per login session prompts.
        home.sessionVariables.GITSIGN_CREDENTIAL_CACHE = gitsignCacheSocket;

        programs.git = {
          settings.url."git@github-work:nellotech/".insteadOf = "git@github.com:nellotech/";

          includes = [
            {
              condition = "gitdir:${config.xdg.userDirs.extraConfig.WORK}/";
              path = config.sops.templates.work_gitconfig.path;
            }
          ];
        };
      }

      # TODO: this credential-cache setup (launchd agent + cache dir) is
      # Darwin-only; generalize it to other platforms (e.g. systemd user
      # service on Linux) if we ever need gitsign there too.
      (mkIf pkgs.stdenv.hostPlatform.isDarwin {
        home.file."Library/Caches/sigstore/gitsign/.keep".text = "";

        launchd.agents.gitsign-credential-cache = {
          enable = true;
          config = {
            Label = "org.sigstore.gitsign-credential-cache";
            ProgramArguments = ["${pkgs.gitsign}/bin/gitsign-credential-cache"];
            RunAtLoad = true;
            KeepAlive = true;
          };
        };
      })
    ]))
  ]);
}

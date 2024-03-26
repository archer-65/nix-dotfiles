{
  pkgs,
  config,
  options,
  lib,
  ...
}:
with lib; let
  cfg = config.system.modules.credentials.gpg;
in {
  options.system.modules.credentials.gpg = {
    enable = mkEnableOption "gpg configuration";
  };

  config = mkIf cfg.enable {
    programs.gnupg.agent =
      {
        enable = true;
        enableSSHSupport = true;
        pinentryPackage = "gnome3";
      }
      // lib.optionalAttrs (builtins.hasAttr "pinentryFlavor" config.programs.gnupg.agent) {
        pinentryPackage = "gnome3";
      }
      // lib.optionalAttrs (builtins.hasAttr "pinentryPackage" config.programs.gnupg.agent) {
        pinentryPackage = pkgs.pinentry-gnome3;
      };
  };
}

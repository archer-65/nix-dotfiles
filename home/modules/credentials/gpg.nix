{
  config,
  options,
  lib,
  ...
}:
with lib; let
  cfg = config.user-modules.credentials.gpg;
in {
  options.user-modules.credentials.gpg = {
    enable = mkEnableOption "gpg-agent user configuration";
  };

  config = mkIf cfg.enable {
    services.gpg-agent = {
      enable = true;
      enableSshSupport = true;
      pinentryFlavor = "gnome3";
    };
  };
}

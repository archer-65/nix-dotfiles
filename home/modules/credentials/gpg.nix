{
  config,
  options,
  lib,
  ...
}:
with lib; let
  cfg = config.home.modules.credentials.gpg;
in {
  options.home.modules.credentials.gpg = {
    enable = mkEnableOption "gpg-agent user configuration";
  };

  config = mkIf cfg.enable {
    programs.gpg.enable = true;

    services.gpg-agent = {
      enable = true;
      enableSshSupport = true;
      pinentryFlavor = "gnome3";
    };
  };
}

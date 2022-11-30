{
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
    programs.gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor = "gnome3";
    };
  };
}

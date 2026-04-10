{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.services.printing;
in {
  options.system.modules.services.printing = {
    enable = mkEnableOption "cups";
  };

  config = mkIf cfg.enable {
    # Enable CUPS
    services.printing = {
      enable = true;
      drivers = with pkgs; [
        pkgs.hplipWithPlugin
        pkgs.samsung-unified-linux-driver
        cups-filters
        cups-browsed
      ];
    };

    services.avahi = {
      enable = true;
      nssmdns4 = true;
      openFirewall = true;
    };
  };
}

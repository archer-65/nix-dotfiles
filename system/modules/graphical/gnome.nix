{
  lib,
  config,
  inputs,
  ...
}:
with lib; let
  cfg = config.system.modules.graphical.gnome;
in {
  options.system.modules.graphical.gnome = {
    enable = mkEnableOption "enable gnome";
  };

  config = mkIf cfg.enable {
    services = {
      xserver = {
        desktopManager.gnome.enable = true;
        displayManager.gdm.enable = true;
      };

      # Some redundancy, more control
      gnome = {
        core-utilities.enable = lib.mkForce false;
        gnome-initial-setup.enable = lib.mkForce false;
        gnome-online-accounts.enable = lib.mkForce false;
        gnome-online-miners.enable = lib.mkForce false;
        gnome-browser-connector.enable = lib.mkForce false;
        games.enable = lib.mkForce false;
        evolution-data-server.enable = lib.mkForce false;
      };

      dleyna-renderer.enable = lib.mkForce false;
      dleyna-server.enable = lib.mkForce false;
      geoclue2.enable = lib.mkForce false;
    };

    programs.evince.enable = lib.mkForce false;
    programs.geary.enable = lib.mkForce false;

    environment.systemPackages = with pkgs.gnome; [
      gnome-backgrounds
      gnome-themes-extra
      pkgs.gnome-tour
      pkgs.gnome-user-docs
      pkgs.orca
    ];
  };
}

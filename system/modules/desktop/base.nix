{
  lib,
  config,
  pkgs,
  ...
}:
with lib; let
  cfgDependency = config.modules.desktop.xorg;
in {
  config = mkIf cfgDependency.enable {
    services.gnome.gnome-keyring.enable = true;
    #This should enable a pam `login` module to unlock gnome-keyring automatically after login.
    security.pam.services.greetd.enableGnomeKeyring = true;

    programs.dconf.enable = true;
    services.dbus = {
      enable = true;
      packages = [pkgs.dconf];
    };

    # Trash and GTK apps features
    services.gvfs.enable = true;
    services.tumbler.enable = true;
    services.udisks2.enable = true;

    environment.systemPackages = with pkgs; [
      gnome.seahorse
      libsecret
      pkgs.libinput
    ];

    user.extraGroups = ["video"];
  };
}

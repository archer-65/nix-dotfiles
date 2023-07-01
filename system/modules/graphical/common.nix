{
  lib,
  config,
  pkgs,
  ...
}:
with lib; let
  cfgXorg = config.system.modules.graphical.xorg;
  cfgWayland = config.system.modules.graphical.wayland;
  cfgGnome = config.system.modules.graphical.gnome;
in {
  config = mkIf (cfgXorg.enable || cfgWayland.enable || cfgGnome.enable) {
    services.xserver.excludePackages = [pkgs.xterm];
    services.gnome.gnome-keyring.enable = true;
    # This should enable a pam `login` module to unlock gnome-keyring automatically after login.
    security.pam.services.greetd.enableGnomeKeyring = true;

    security.polkit.enable = true;

    programs.dconf.enable = true;
    services.dbus = {
      enable = true;
      packages = with pkgs; [
        dconf
        gcr # pinentry-gnome3 may not work without this
      ];
    };

    # Trash and GTK apps features
    services.gvfs.enable = true;
    services.tumbler.enable = true;
    services.udisks2.enable = true;

    environment.systemPackages = with pkgs; [
      gnome.seahorse
      libsecret
      libinput
    ];

    primaryUser.extraGroups = ["video"];
  };
}

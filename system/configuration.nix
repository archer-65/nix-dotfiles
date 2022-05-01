# This configuration files contain common settings (of the system)
# between all my machines.

{ config, lib, pkgs, inputs, user,  ... }:

let
  # Overriding nerd fonts (if you don't, all nerd fonts will be installed.)
  nerdFonts = pkgs.nerdfonts.override {
    fonts = [
      "FiraCode"
      "VictorMono"
      "Iosevka"
    ];
  };
in {
  # Here all imports for system-wide modules
  imports = [
    #WM
    ./desktop/qtile.nix
  ];

  # This global flag is deprecated
  networking.useDHCP = false;

  # Timezone
  time.timeZone = "Europe/Rome";

  # Locale
  i18n.defaultLocale = "en_US.UTF-8";

  # System-wide packages
  environment = {
    systemPackages = with pkgs; [
      alacritty
      killall
      git
      wget
      firefox
      vim
      emacs
    ];
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  networking.firewall.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # Console settings
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # System fonts
  fonts.fonts = with pkgs; [
    source-code-pro
    font-awesome
    corefonts
    nerdFonts
    source-han-sans
  ];

  # Global services
  services = {
    # Xorg
    xserver = {
      enable = true;

      # Enable XFCE and Lightdm
      displayManager.lightdm.enable = true;
      desktopManager.xfce.enable = true;
    };

    # SSH and SSH Daemon
    openssh = {
      enable = true;
    };
    sshd.enable = true;


    # Pipewire settings
    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };

    # Flatpak settings
    flatpak.enable = true;

    # Enable CUPS
    printing = {
      enable = true;
      drivers = [ pkgs.hplip ];
    };
  };

  # XDG Portals, useful for wayland screen sharing and flatpak).
  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  };

  # Users configuration
  users.users.${user} = {
    isNormalUser = true;
    extraGroups = [ "wheel" "video" "audio" "networkmanager" "kvm" "libvirtd" "plex" ];
  };
  
  # NixOS GC, Upgrades and Flakes
  nix = {
    settings.auto-optimise-store = true;

    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };

    package = pkgs.nixFlakes;
    registry.nixpkgs.flake = inputs.nixpkgs;
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs          = true
      keep-derivations      = true
    '';
  };
  nixpkgs.config.allowUnfree = true;

  system = {
    autoUpgrade = {
      enable = true;
      channel = "https://nixos.org/channels/nixos-unstable";
    };
    stateVersion = "22.05";
  };
}


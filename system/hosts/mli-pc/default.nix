{
  pkgs,
  config,
  lib,
  ...
}: {
  imports = [./hardware-configuration.nix ./options.nix];

  # Kernel related
  boot.kernelPackages = pkgs.linuxPackages_latest.extend (lfinal: lprev: {
    # TODO: Remove when https://github.com/DisplayLink/evdi/issues/489 is resolved
    evdi = lprev.evdi.overrideAttrs (efinal: eprev: rec {
      version = "1.14.7";

      src = pkgs.fetchFromGitHub {
        owner = "DisplayLink";
        repo = "evdi";
        rev = "refs/tags/v${version}";
        hash = "sha256-z3GawjaokbmmUC1LihwGSnF3tUp9n/FO+kDiWvBq+mY=";
      };

      patches = [
	(pkgs.fetchpatch {
          name = "dont-allow-mmap-imported-gem-objects.patch";
          url = "https://github.com/DisplayLink/evdi/commit/3323f3190dc922f1b4ad5f525f09b72afd2739e0.diff";
          sha256 = "sha256-KT3E+Pe0Iw6+BNjPgXbi7/igwNSvGzs0YO7HLv3px+0=";
          revert = true;
        })
      ];
    });
  });

  boot.kernelModules = [];
  boot.kernelParams = ["intel_pstate=active" "zswap.enabled=0" "i915.force_probe=46a8"];

  boot.initrd.kernelModules = [];
  boot.initrd.availableKernelModules = [];

  # General EFI settings
  boot.loader.efi = {
    canTouchEfiVariables = true;
  };
  boot.loader.timeout = 5;

  boot.loader = {
    systemd-boot = {
      enable = true;
      consoleMode = "max";
    };
  };

  networking.interfaces.enp1s0.useDHCP = true;
  networking.interfaces.wlp0s20f3.useDHCP = true;

  # Graphics
  environment.variables = {
    VDPAU_DRIVER = lib.mkIf config.hardware.graphics.enable (lib.mkDefault "va_gl");
  };

  hardware.graphics.extraPackages = with pkgs; [
    (
      if (lib.versionOlder (lib.versions.majorMinor lib.version) "23.11") then
        vaapiIntel
      else
        intel-vaapi-driver
    )
    intel-media-driver
  ];

  hardware.graphics.extraPackages32 = with pkgs.driversi686Linux; [
    (
      if (lib.versionOlder (lib.versions.majorMinor lib.version) "23.11") then
        vaapiIntel
      else
        intel-vaapi-driver
    )
    intel-media-driver
  ];


  # Power management
  services.power-profiles-daemon.enable = true;
  services.thermald.enable = true;
  services.udev.extraRules = ''
    ACTION=="change", SUBSYSTEM=="power_supply", ENV{POWER_SUPPLY_ONLINE}=="0", RUN+="${pkgs.power-profiles-daemon}/bin/powerprofilesctl set power-saver"
    ACTION=="change", SUBSYSTEM=="power_supply", ENV{POWER_SUPPLY_ONLINE}=="1", RUN+="${pkgs.power-profiles-daemon}/bin/powerprofilesctl set balanced"
  '';

  # Firmware updater
  services.fwupd.enable = true;

  services.xserver = {
    enable = true;
    # Disable `lightdm` because it is enabled by default sometimes (e.g. greetd with also `xserver` option enabled).
    displayManager.lightdm.enable = lib.mkForce false;
  };

  zramSwap = {
    enable = true;
    memoryPercent = 40;
    priority = 10;
  };

  environment.systemPackages = [pkgs.compsize];
}

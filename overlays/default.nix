inputs:
let
  # Pass flake inputs to overlay so we can use the sources pinned in flake.lock
  # instead of having to keep sha256 hashes in each package for src
  inherit inputs;
  inherit (inputs.self) pkgs;
in _self: super: {
  scripts = {
    volume = super.callPackage ../packages/scripts/volume.nix { };
    usedram = super.callPackage ../packages/scripts/usedram.nix { };
    usedcpu = super.callPackage ../packages/scripts/usedcpu.nix { };
    hwmon_devices = super.callPackage ../packages/scripts/hwmon_devices.nix { };
  };

  scripts.rofi = {
    emoji = super.callPackage ../packages/rofi/emoji.nix { };
    greenclip = super.callPackage ../packages/rofi/greenclip.nix { };
    launcher = super.callPackage ../packages/rofi/launcher.nix { };
    powermenu = super.callPackage ../packages/rofi/powermenu.nix { };
  };

  rofi-rbw = super.callPackage ../packages/rofi-rbw.nix {
    pypkgs = pkgs.python39Packages;
  };

  swhkd = super.callPackage ../packages/swhkd.nix { };

  # OVERLAYS (overrideAttrs)
  discord = super.discord.overrideAttrs (old: rec {
    version = "0.0.18";
    src = super.fetchurl {
      url = "https://dl.discordapp.net/apps/linux/${version}/discord-${version}.tar.gz";
      sha256 = "BBc4n6Q3xuBE13JS3gz/6EcwdOWW57NLp2saOlwOgMI=";
    };
  });
}

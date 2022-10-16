inputs: self: super:
let
  inherit (super) system;
  pkgs =
    inputs.nixpkgs.legacyPackages.${system}.extend inputs.self.overlays.default;
in {
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

  rofi-rbw = super.callPackage ../packages/rofi/rofi-rbw.nix {
    pypkgs = pkgs.python39Packages;
  };

  swhkd = super.callPackage ../packages/swhkd { };

  # OVERLAYS (overrideAttrs)
  discord = super.discord.overrideAttrs (old: rec {
    version = "0.0.18";
    src = super.fetchurl {
      url =
        "https://dl.discordapp.net/apps/linux/${version}/discord-${version}.tar.gz";
      sha256 = "BBc4n6Q3xuBE13JS3gz/6EcwdOWW57NLp2saOlwOgMI=";
    };
  });

  tdlib = super.tdlib.overrideAttrs (old: rec {
    version = "1.8.7";
    src = pkgs.fetchFromGitHub {
      owner = "tdlib";
      repo = "td";
      rev = "a7a17b34b3c8fd3f7f6295f152746beb68f34d83";
      sha256 = "sha256-Rv96dZPQruJlXoZaQQ8QWbjEdvYljE9XjDUfa2cCpig=";
    };
  });
}

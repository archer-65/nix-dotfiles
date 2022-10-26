inputs: self: super:
let
  inherit (super) system;
  pkgs =
    inputs.nixpkgs.legacyPackages.${system}.extend inputs.self.overlays.default;
  pkgs-stable = inputs.nixpkgs-2205.legacyPackages.${system};
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

  # Not needed since 1.8.7 commit on nixpkgs
  # tdlib = super.tdlib.overrideAttrs (old: rec {
  #   version = "1.8.7";
  #   src = pkgs.fetchFromGitHub {
  #     owner = "tdlib";
  #     repo = "td";
  #     rev = "a7a17b34b3c8fd3f7f6295f152746beb68f34d83";
  #     sha256 = "sha256-Rv96dZPQruJlXoZaQQ8QWbjEdvYljE9XjDUfa2cCpig=";
  #   };
  # });

  # See https://github.com/NixOS/nixpkgs/issues/197408
  # and https://github.com/altdesktop/python-dbus-next/issues/135
  python3Packages = super.python3Packages.override {
    overrides = pfinal: pprev: {
      dbus-next = pprev.dbus-next.overridePythonAttrs (old: {
        checkPhase = builtins.replaceStrings ["not test_peer_interface"] ["not test_peer_interface and not test_tcp_connection_with_forwarding"] old.checkPhase;
      });
    };
  };
}

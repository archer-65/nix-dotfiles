inputs: final: prev: let
  inherit (prev) system;
  pkgs-latest = inputs.nixpkgs.legacyPackages.${system};
  pkgs-stable = inputs.nixpkgs-stable.legacyPackages.${system};
in {
  scripts = {
    volume = prev.callPackage ./scripts/volume.nix {};
    usedram = prev.callPackage ./scripts/usedram.nix {};
    usedcpu = prev.callPackage ./scripts/usedcpu.nix {};
    hwmon_devices = prev.callPackage ./scripts/hwmon_devices.nix {};
  };

  scripts.rofi = {
    emoji = prev.callPackage ./rofi/emoji.nix {};
    greenclip = prev.callPackage ./rofi/greenclip.nix {};
    launcher = prev.callPackage ./rofi/launcher.nix {};
    powermenu = prev.callPackage ./rofi/powermenu.nix {};
  };

  # OVERLAYS (overrideAttrs)
  discord = prev.discord.overrideAttrs (old: rec {
    version = "0.0.18";
    src = prev.fetchurl {
      url = "https://dl.discordapp.net/apps/linux/${version}/discord-${version}.tar.gz";
      sha256 = "BBc4n6Q3xuBE13JS3gz/6EcwdOWW57NLp2saOlwOgMI=";
    };
  });

  # See https://github.com/hashicorp/terraform-ls/issues/1067
  terraform-ls = prev.terraform-ls.overrideAttrs (old: rec {
    pname = "terraform-ls";
    version = "0.28.1";
    src = prev.fetchFromGitHub {
      owner = "hashicorp";
      repo = pname;
      rev = "v${version}";
      sha256 = "sha256-CYbeRhwoffyELM0REZL14m4tTe/66GDToqNKcEfmums=";
    };
  });

  # Not needed since 1.8.7 commit on nixpkgs
  # tdlib = prev.tdlib.overrideAttrs (old: rec {
  #   version = "1.8.7";
  #   src = prev.fetchFromGitHub {
  #     owner = "tdlib";
  #     repo = "td";
  #     rev = "a7a17b34b3c8fd3f7f6295f152746beb68f34d83";
  #     sha256 = "sha256-Rv96dZPQruJlXoZaQQ8QWbjEdvYljE9XjDUfa2cCpig=";
  #   };
  # });
}

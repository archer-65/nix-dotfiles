# With inputs as an argument it is possible to access to the flake itself. This could be helpful in future.
inputs: {
  # Overlays for personal pkgs (callPackage)
  additions = final: _: import ../packages {pkgs = final;};


  # Overlays for various pkgs (e.g. broken, not updated)
  modifications = final: prev: {
    # discord = prev.discord.overrideAttrs (old: rec {
    #   version = "0.0.20";

    #   src = final.fetchurl {
    #     url = "https://dl.discordapp.net/apps/linux/${version}/discord-${version}.tar.gz";
    #     sha256 = "BBc4n6Q3xuBE13JS3gz/6EcwdOWW57NLp2saOlwOgMI=";
    #   };
    # });

    swaylock-effects = prev.swaylock-effects.overrideAttrs (old: rec {
      pname = "swaylock-effects";
      version = "1.6.10";

      src = final.fetchFromGitHub {
        owner = "jirutka";
        repo = "swaylock-effects";
        rev = "b2736c5bef3add118183654305d05903c5947668";
        sha256 = "sha256-umxEwegKuJd/DUjaUQ88lbcQNxSY99yepBnQaFr3fDI=";
      };
    });

    waybar = prev.waybar.overrideAttrs (oldAttrs: {
      mesonFlags = oldAttrs.mesonFlags ++ [ "-Dexperimental=true" ];
    });

    notmuch-mailmover = inputs.notmuch-mailmover.packages.x86_64-linux.default;

    # Not needed since 1.8.7 commit on nixpkgs
    # tdlib = prev.tdlib.overrideAttrs (old: rec {
    #   version = "1.8.7";
    #   src = final.fetchFromGitHub {
    #     owner = "tdlib";
    #     repo = "td";
    #     rev = "a7a17b34b3c8fd3f7f6295f152746beb68f34d83";
    #     sha256 = "sha256-Rv96dZPQruJlXoZaQQ8QWbjEdvYljE9XjDUfa2cCpig=";
    #   };
    # });
  };
}

# With inputs as an argument it is possible to access to the flake itself. This could be helpful in future.
inputs: {
  # Overlays for personal pkgs (callPackage)
  additions = final: _: import ../packages {pkgs = final;};

  # Overlays for various pkgs (e.g. broken, not updated)
  modifications = final: prev: {
    discord = prev.discord.overrideAttrs (old: rec {
      version = "0.0.20";

      src = final.fetchurl {
        url = "https://dl.discordapp.net/apps/linux/${version}/discord-${version}.tar.gz";
        sha256 = "BBc4n6Q3xuBE13JS3gz/6EcwdOWW57NLp2saOlwOgMI=";
      };
    });

    lieer = prev.lieer.overrideAttrs (old: rec {
      pname = "lieer";
      # version = "244c9bfe11d87cd8a09c38ef5470e798ad41359e";

      # src = prev.fetchFromGitHub {
      #   owner = "gauteh";
      #   repo = "lieer";
      #   rev = version;
      #   sha256 = "sha256-CaHI8sdM1jBubszjqaOkxaDA2zZxwufgjFeDkuTHRIo=";
      # };

      # propagatedBuildInputs = old.propagatedBuildInputs;

      patches = (old.patches or []) ++ [ ./gmi-init.patch ];
    });

    # See https://github.com/hashicorp/terraform-ls/issues/1067
    # terraform-ls = prev.terraform-ls.overrideAttrs (old: rec {
    #   pname = "terraform-ls";
    #   version = "0.28.1";
    #   src = final.fetchFromGitHub {
    #     owner = "hashicorp";
    #     repo = pname;
    #     rev = "v${version}";
    #     sha256 = "sha256-CYbeRhwoffyELM0REZL14m4tTe/66GDToqNKcEfmums=";
    #   };
    # });

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

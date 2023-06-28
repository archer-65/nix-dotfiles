# With inputs as an argument it is possible to access to the flake itself. This could be helpful in future.
# Thanks to Misterio77 for these functions!!!
{
  outputs,
  inputs,
}: let
  addPatches = pkg: patches:
    pkg.overrideAttrs (oldAttrs: {
      patches = (oldAttrs.patches or []) ++ patches;
    });
in {
  # For every flake input, aliases 'pkgs.inputs.${flake}' to
  # 'inputs.${flake}.packages.${pkgs.system}' or
  # 'inputs.${flake}.legacyPackages.${pkgs.system}' or
  flake-inputs = final: _: {
    inputs =
      builtins.mapAttrs
      (_: flake: (flake.legacyPackages or flake.packages or {}).${final.system} or {})
      inputs;
  };

  # Overlays for personal pkgs (callPackage)
  additions = final: _: import ../packages {pkgs = final;};

  # Overlays for various pkgs (e.g. broken, not updated)
  modifications = final: prev: rec {
    stable = import inputs.nixpkgs-stable {
      inherit (final) system;
      config.allowUnfree = true;
    };

    waybar = prev.waybar.overrideAttrs (oldAttrs: {
      mesonFlags = oldAttrs.mesonFlags ++ ["-Dexperimental=true"];
    });

    lieer = prev.lieer.overrideAttrs (oldAttrs: rec {
      pname = "lieer";
      version = "1.4";

      src = final.fetchFromGitHub {
        owner = "gauteh";
        repo = "lieer";
        rev = "v${version}";
        sha256 = "sha256-2LujfvsxMHHmYjYOnLJaLdSlzDeej+ehUr4YfVe903U=";
      };

      propagatedBuildInputs = with final.pkgs.python3Packages; [
        notmuch2
        oauth2client
        google-api-python-client
        tqdm
        setuptools
      ];
    });

    # Keep this if telega is borked
    # tdlib = prev.tdlib.overrideAttrs (oldAttrs: {
    #   version = "1.8.8";
    #   src = final.fetchFromGitHub {
    #     owner = "tdlib";
    #     repo = "td";
    #     rev = "bbe37ee594d97f3c7820dd23ebcd9c9b8dac51a0";
    #     sha256 = "sha256-jLJglvq+7f+zCoanDRTFpUsH/M1Qf7PWJ1JjvmZsa24==";
    #   };
    # });

    # Wait until https://github.com/NixOS/nixpkgs/pull/232415 is merged
    linuxPackages_latest = let
      python3WithLibs = final.python3.withPackages (ps:
        with ps; [
          pybind11
        ]);
    in
      prev.linuxPackages_latest.extend (self: super: {
        evdi = super.evdi.overrideAttrs (oldAttrs: rec {
          pname = "evdi";
          version = "1.13.1";

          src = final.fetchFromGitHub {
            owner = "DisplayLink";
            repo = pname;
            rev = "v${version}";
            sha256 = "sha256-Or4hhnFOtC8vmB4kFUHbFHn2wg/NsUMY3d2Tiea6YbY=";
          };

          buildInputs = with final.pkgs; [python3WithLibs] ++ oldAttrs.buildInputs;

          patches = [
            ./0000-fix-drm-path.patch
          ];
        });
      });

    displaylink =
      (prev.displaylink.overrideAttrs (oldAttrs: {
        pname = "displaylink";
        version = "5.7.0-61.129";

        src = final.requireFile rec {
          name = "displaylink-570.zip";
          sha256 = "807f1c203ac1e71c6f1f826493b9bb32e277f07cb2cf48537bf8cfdc68dd1515";
          message = ''
              In order to install the DisplayLink drivers, you must first
              comply with DisplayLink's EULA and download the binaries and
              sources from here:
              https://www.synaptics.com/products/displaylink-graphics/downloads/ubuntu-5.6.1
              https://www.synaptics.com/products/displaylink-graphics/downloads/ubuntu-5.7
              Once you have downloaded the file, please use the following
              commands and re-run the installation:
              mv \$PWD/"DisplayLink USB Graphics Software for Ubuntu5.6.1-EXE.zip" \$PWD/${name}
            #   mv \$PWD/"DisplayLink USB Graphics Software for Ubuntu5.7-EXE.zip" \$PWD/${name}
            #   nix-prefetch-url file://\$PWD/${name}
            # '';
        };
        unpackPhase = let
          version = "5.7.0-61.129";
        in ''
          unzip $src
          chmod +x displaylink-driver-${version}.run
          ./displaylink-driver-${version}.run --target . --noexec --nodiskspace
        '';
      }))
      .override {
        inherit (final.linuxPackages_latest) evdi;
      };

    wlroots = prev.wlroots.overrideAttrs (oldAttrs: {
      patches = (oldAttrs.patches or []) ++ [./displaylink.patch];
    });
  };
}

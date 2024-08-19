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
    waybar = prev.waybar.overrideAttrs (oldAttrs: {
      mesonFlags = oldAttrs.mesonFlags ++ ["-Dexperimental=true"];
    });

    rofi-emoji-wayland = prev.rofi-emoji.overrideAttrs (oldAttrs: rec {
      buildInputs = with final; [
        rofi-wayland-unwrapped
        cairo
        glib
        libnotify
        wl-clipboard
        xclip
        xsel
      ];
    });

    sway-displaylink = let
      wlroots-sway = prev.wlroots.overrideAttrs (_: {
        # https://gitlab.freedesktop.org/wlroots/wlroots/-/merge_requests/4640
        patches = [
           (prev.fetchpatch {
              name = "scannout-without-mgpu-renderer.patch";
              url = "https://gitlab.freedesktop.org/-/project/12103/uploads/839d4179d715217513c5c6a9f7ae1963/scannout-without-mgpu-renderer.patch";
              sha256 = "1wkw93zla6ddk5c6g01k0j4sv51wcl3szbd7n8q4hh2ffawv8f49";
            })
        ];
      });

      sway-unwrapped =
        (prev.sway-unwrapped.overrideAttrs (o: {
          src = prev.fetchFromGitHub {
            owner = "swaywm";
            repo = "sway";
            rev = "bc258a3be2f946c1c93bcbe40735b2db068e0ea8";
            sha256 = "sha256-FTzDk2t1u3ckhBSLKB+rJSofc5wBYh3rGUffHkLRDco=";
          };
        }))
        .override {
          wlroots = wlroots-sway;
        };
    in
      prev.sway.override {
        inherit sway-unwrapped;
        extraOptions = ["--unsupported-gpu"];
      };

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
  };
}

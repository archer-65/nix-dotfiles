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
        # https://gitlab.freedesktop.org/wlroots/wlroots/-/merge_requests/4824
        patches = [
          (prev.fetchpatch {
            name = "scannout-without-mgpu-renderer.patch";
            url = "https://gitlab.freedesktop.org/wlroots/wlroots/-/merge_requests/4824.patch";
            sha256 = "19phmcplc1y2rvhvgi6a2vkzflkf9b2xzlyb58dvbffl87vgv224";
          })
        ];
      });

      sway-unwrapped = prev.sway-unwrapped.override {
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

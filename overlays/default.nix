inputs:
let
  # Pass flake inputs to overlay so we can use the sources pinned in flake.lock
  # instead of having to keep sha256 hashes in each package for src
  inherit inputs;
in self: super: {
  # Example package, used only for tests
  scripts.volume = super.callPackage ../packages/volume.nix { };

  scripts = {
    rofi.usedcpu = super.callPackage ../packages/usedcpu.nix { };
    rofi.usedram = super.callPackage ../packages/usedram.nix { };
    rofi.emoji = super.callPackage ../packages/emoji.nix { };
    rofi.greenclip = super.callPackage ../packages/greenclip.nix { };
    rofi.launcher = super.callPackage ../packages/launcher.nix { };
    rofi.powermenu = super.callPackage ../packages/powermenu.nix { };
  };
}

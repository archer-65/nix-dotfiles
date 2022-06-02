inputs:
let
  # Pass flake inputs to overlay so we can use the sources pinned in flake.lock
  # instead of having to keep sha256 hashes in each package for src
  inherit inputs;
in self: super: {
  # Example package, used only for tests
  # scripts = {
  #   volume = super.callPackage ../packages/scripts/volume.nix { };
  #   usedram = super.callPackage ../packages/scripts/usedram.nix { };
  #   usedcpu = super.callPackage ../packages/scripts/usedcpu.nix { };
  # };

  # scripts.rofi = {
  #   emoji = super.callPackage ../packages/rofi/emoji.nix { };
  #   greenclip = super.callPackage ../packages/rofi/greenclip.nix { };
  #   launcher = super.callPackage ../packages/rofi/launcher.nix { };
  #   powermenu = super.callPackage ../packages/rofi/powermenu.nix { };
  # };

  # rofi-rbw = super.callPackage ../packages/rofi-rbw.nix { };
}

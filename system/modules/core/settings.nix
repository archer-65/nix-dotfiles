{pkgs, ...}: {
  # NixOS GC, Upgrades and Flakes
  nix = {
    settings.auto-optimise-store = true;

    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };

    package = pkgs.nixVersions.stable;
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs          = true
      keep-derivations      = true
      warn-dirty = false
    '';
  };

  # system = {
  #   autoUpgrade = {
  #     enable = true;
  #     channel = "https://nixos.org/channels/nixos-unstable";
  #   };
  #   stateVersion = "22.05";
  # };
}

{ lib, config, pkgs, ... }: 
  
  {
  # NixOS GC, Upgrades and Flakes
  nix = {
    trustedUsers = [ "root" "mario" ];   

    settings.auto-optimise-store = true;

    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };

    package = pkgs.nixFlakes;
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

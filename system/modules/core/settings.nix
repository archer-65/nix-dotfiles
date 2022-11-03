{pkgs, ...}: {
  # NixOS GC, Upgrades and Flakes
  nix = {
    settings = {
      auto-optimise-store = true;
      trusted-users = ["root" "@wheel "];
      allowed-users = ["root" "@wheel "];
    };

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
}

let
  inherit (builtins) attrNames concatMap listToAttrs;

  filterAttrs = pred: set:
    listToAttrs (concatMap (name:
      let value = set.${name};
      in if pred name value then [{ inherit name value; }] else [ ])
      (attrNames set));

  configurations = {
    # System configurations
    quietfrost = {
      type = "nixos";
      localSystem = "x86_64-linux";
      stateVersion = "22.05";
    };
    mate = {
      type = "nixos";
      localSystem = "x86_64-linux";
      stateVersion = "22.05";
    };
    vm = {
      type = "nixos";
      localSystem = "x86_64-linux";
      stateVersion = "22.05";
    };

    # Home-manager standalone
    "mario@quietfrost" = rec {
      type = "home-manager";
      localSystem = "x86_64-linux";
      username = "mario";
      stateVersion = "22.05";
    };
    "mario@mate" = rec {
      type = "home-manager";
      localSystem = "x86_64-linux";
      username = "mario";
      stateVersion = "22.05";
    };
    "mario@vm" = rec {
      type = "home-manager";
      localSystem = "x86_64-linux";
      username = "mario";
      stateVersion = "22.05";
    };
  };
in {
  all = configurations;

  nixos = rec {
    all = filterAttrs (_: v: v.type == "nixos") configurations;
    x86_64-linux = filterAttrs (_: v: v.localSystem == "x86_64-linux") all;
    aarch64-linux = filterAttrs (_: v: v.localSystem == "aarch64-linux") all;
  };

  homeManager = rec {
    all = filterAttrs (_: v: v.type == "home-manager") configurations;
    x86_64-linux = filterAttrs (_: v: v.localSystem == "x86_64-linux") all;
    aarch64-linux = filterAttrs (_: v: v.localSystem == "aarch64-linux") all;
  };
}

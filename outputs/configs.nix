lib: let
  inherit (lib) filterAttrs;

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
    "mario@quietfrost" = {
      type = "home-manager";
      localSystem = "x86_64-linux";
      username = "mario";
      stateVersion = "22.05";
    };
    "mario@mate" = {
      type = "home-manager";
      localSystem = "x86_64-linux";
      username = "mario";
      stateVersion = "22.05";
    };
    "mario@vm" = {
      type = "home-manager";
      localSystem = "x86_64-linux";
      username = "mario";
      stateVersion = "22.05";
    };
  };
in {
  # Get nixos configurations set
  nixos = filterAttrs (_: v: v.type == "nixos") configurations;

  # Get home-manager configurations set
  home-manager = filterAttrs (_: v: v.type == "home-manager") configurations;
}

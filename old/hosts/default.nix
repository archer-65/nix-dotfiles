inputs:
let
  system = "x86_64-linux";
  user = "mario";
  inherit (inputs) self;
  inherit (self) pkgs;

  sharedModules = [
    { _module.args = { inherit inputs; }; }
    ./common
    inputs.home-manager.nixosModules.home-manager
    {
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
    }
  ];

  inherit (self.lib) nixosSystem makeOverridable;
  inherit (import "${self}/home/profiles" inputs) homeImports;
in {
  ### LAPTOP ###
  mate = nixosSystem {
    inherit system;
    specialArgs = { inherit user inputs; };
    modules = [
      ./mate
      { home-manager.users.${user}.imports = homeImports."mario@mate"; }
    ] ++ sharedModules;
  };

  ### VM ###
  vm = nixosSystem {
    inherit system;
    specialArgs = { inherit user inputs; };
    modules =
      [ ./vm { home-manager.users.${user}.imports = homeImports."mario@vm"; } ]
      ++ sharedModules;
  };
}

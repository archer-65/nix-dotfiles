inputs:
let
  inherit (inputs) self;
  inherit (self) pkgs;
  inherit (self.lib) mkHome;
  user = "mario";
  isDesktop = false;

  homeModule = import ../home.nix {
    inherit pkgs;
    inherit (pkgs) config lib;
    inherit user;
    isDesktop = true;
  };

  sharedModules = [
    ../home.nix
    #homeModule
  ];

  homeImports = {
    "mario@mate" = sharedModules;
    "mario@vm" = sharedModules;
  };

in {
  inherit homeImports;

  homeConfigurations = {
    "mario@mate" = mkHome {
      username = user;
      extraModules = homeImports."mario@mate";
    };

    "mario@vm" = mkHome {
      username = user;
      extraModules = homeImports."mario@vm";
    };
  };
}

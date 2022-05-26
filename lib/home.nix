inputs:
let
  inherit (inputs) self;
  inherit (inputs.home-manager.lib) homeManagerConfiguration;

  mkHome = args:
    homeManagerConfiguration (args // {
      homeDirectory = "/home/${args.username}";
      pkgs = inputs.self.pkgs.${args.system};
    });
in { inherit mkHome; }

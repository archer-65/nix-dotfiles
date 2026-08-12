{inputs, ...}:
with builtins; let
  inherit (inputs) self nixpkgs-darwin darwin home-manager-darwin;
  inherit (self) outputs overlays;

  genConfiguration = {
    username,
    hostname,
    system,
    stateVersion,
    homeStateVersion,
    ...
  }: let
    pkgs = import nixpkgs-darwin {
      inherit system;
      config.allowUnfree = true;
      overlays = attrValues overlays;
    };
  in
    darwin.lib.darwinSystem {
      inherit system;
      inherit pkgs;

      modules = [
        inputs.determinate.darwinModules.default
        {
          system.stateVersion = stateVersion;
          users.users."${username}" = {
            name = "${username}";
            home = "/Users/${username}";
          };
          system.primaryUser = username;

          # https://determinate.systems/posts/nix-darwin-updates/
          # https://docs.determinate.systems/guides/nix-darwin/
          nix.enable = false;
          determinateNix.enable = true;
          determinateNix.customSettings.trusted-users = ["${username}"];
        }

        home-manager-darwin.darwinModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.extraSpecialArgs = {inherit inputs outputs;};
          home-manager.users."${username}" = {...}: {
            imports = (builtins.attrValues self.outputs.homeModules.mario) ++ ["${self}/home/${username}/hosts/${hostname}.nix"];

            home = {
              username = "${username}";
              stateVersion = homeStateVersion;
              homeDirectory = "/Users/${username}";
            };
          };
        }

        "${self}/system/hosts/${hostname}"
      ];

      specialArgs = {inherit inputs outputs;};
    };
in
  genConfiguration

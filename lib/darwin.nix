{inputs, ...}:
with builtins; let
  inherit (inputs) self nixpkgs-darwin darwin home-manager;
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
        {
          system.stateVersion = stateVersion;
          users.users."${username}" = {
            name = "${username}";
            home = "/Users/${username}";
          };
          system.primaryUser = username;

          # https://determinate.systems/posts/nix-darwin-updates/
          nix.enable = false;
          nix.settings.trusted-users = ["${username}"];
        }

        inputs.mac-app-util.darwinModules.default

        home-manager.darwinModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.extraSpecialArgs = {inherit inputs outputs;};
          home-manager.sharedModules = [inputs.mac-app-util.homeManagerModules.default];
          home-manager.users."${username}" = {config, ...}: {
            imports = (builtins.attrValues self.outputs.homeModules.mario) ++ ["${self}/home/${username}/hosts/${hostname}.nix"];

            home = {
              username = "${username}";
              stateVersion = homeStateVersion;
              homeDirectory = "/Users/${username}";
            };

            # TODO: Improve this sh*t
            xdg.userDirs.extraConfig.XDG_WORK_DIR = "${config.home.homeDirectory}/work";
          };
        }

        "${self}/system/hosts/${hostname}"
      ];

      specialArgs = {inherit inputs outputs;};
    };
in
  genConfiguration

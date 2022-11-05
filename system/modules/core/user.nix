{
  config,
  lib,
  options,
  ...
}:
with lib; {
  options = with types; {
    primaryUser = mkOption {
      type = attrs;
      default = {};
      example = {packages = [pkgs.neovim];};
      description = ''
        An alias for passing option to the default user via the option
        <option>users.user.<name></option>.
      '';
    };
  };

  config = {
    # Defines the default user.
    primaryUser = {
      name = "mario";
      home = "/home/mario";
      isNormalUser = true;
      group = "users";
      initialPassword = "nixos";
      uid = 1000;
      extraGroups = ["wheel"];
    };

    users.users.${config.primaryUser.name} = mkAliasDefinitions options.primaryUser;

    # Change me later!
    users.users.root.initialPassword = "nixos";

    primaryUser.openssh.authorizedKeys.keys = let
      yubikey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBR9GjKkCrbAbfuQJXuMTh1I6agrhiHrxlEXhWgidvLS";
    in [
      yubikey
    ];

    environment = {
      sessionVariables = {
        XDG_CACHE_HOME = "$HOME/.cache";
        XDG_CONFIG_HOME = "$HOME/.config";
        XDG_DATA_HOME = "$HOME/.local/share";
        XDG_BIN_HOME = "$HOME/.local/bin";
      };
    };
  };
}

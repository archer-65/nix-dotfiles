{ config, inputs, lib, options, ... }:

with lib;
{
  options = with types; {
    user = mkOption {
      type = attrs;
      default = {};
      example = {
        packages = [ pkgs.neovim ];
      };
      description = ''
        An alias for passing option to the default user via the option
        <option>users.user.<name></option>.
      '';
    };

    localMachine = mkOption {
      type = bool;
      default = true;
      description = ''
        Whether this is a local machine or not. On local machine we can for
        example enable the <literal>noPass = true;</literal> rule in
        <option>security.doas.extraRules</option> for the user, or several
        other relaxations that are not recommended for a server environment.
      '';
    };
  };

  config = {
    # Defines the default user.
    user =
      let
        defaultName = "mario";
        user = builtins.getEnv "USER";
        name = if elem user [ "" "root" ] then defaultName else user;
      in {
        inherit name;
        isNormalUser = true;
        home = "/home/${name}";
        group = "users";
        uid = 1000;
        # If we are on a local machine the ‹wheel› group is not necessary
        # because a special ‹doas› rule will be created.
        extraGroups = if !config.localMachine then [ "wheel" ] else [];
      };

    users.users.${config.user.name} = mkAliasDefinitions options.user;

    nix = let users = [ "root" config.user.name ]; in {
      trustedUsers = users;
      allowedUsers = users;
    };

    system.stateVersion = lib.mkDefault "22.05";

    environment = {
      sessionVariables = {
        XDG_CACHE_HOME  = "$HOME/.cache";
        XDG_CONFIG_HOME = "$HOME/.config";
        XDG_DATA_HOME   = "$HOME/.local/share";
        XDG_BIN_HOME    = "$HOME/.local/bin";
        # To prevent firefox from creating ~/Desktop
        XDG_DESKTOP_DIR = "$HOME";
      };
      variables = {
        # Make some programs "XDG" compliant
        LESSHISTFILE    = "$XDG_CACHE_HOME/lesshst";
        WGETRC          = "$XDG_CONFIG_HOME/wgetrc";
      };
    };
  };
}
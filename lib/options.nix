inputs:
let
  inherit (inputs.nixpkgs) lib;
in with lib; {
  options.specs = {
    enable = mkEnableOption "Enable specific options for multiple configurations";
    font = mkOption {
      default = "VictorMono Nerd Font";
      type = types.str;
    };

    fontSize = mkOption {
      default = 14;
      type = types.int;
    };
  };
}

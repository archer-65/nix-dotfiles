_:
{ options, config, lib, pkgs, ... }:

with lib;

let cfg = config.user-modules.editors.intellij;
in {
  options.user-modules.editors.intellij = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs.jetbrains; [ idea-community ];
  };
}

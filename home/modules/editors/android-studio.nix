{ options, config, lib, pkgs, ... }:

with lib;

let cfg = config.user-modules.editors.android;
in {
  options.user-modules.editors.android = {
    enable = _.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ android-studio ];
  };
}

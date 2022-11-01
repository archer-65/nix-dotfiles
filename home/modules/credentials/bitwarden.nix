{ config, options, lib, pkgs, ... }:

with lib;
let cfg = config.user-modules.credentials.bitwarden;
in {
  options.user-modules.credentials.bitwarden = with types; {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [ rbw ];

    xdg.configFile."rbw/config.json".text = ''
      {
        "email" : "mario.liguori.056@gmail.com",
        "pinentry" : "${pkgs.pinentry-gnome}/bin/pinentry-gnome3"
      }
    '';
  };
}

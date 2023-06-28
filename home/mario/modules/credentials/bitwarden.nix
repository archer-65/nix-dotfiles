{
  config,
  options,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.credentials.bitwarden;
in {
  options.mario.modules.credentials.bitwarden = {
    enable = mkEnableOption "bitwarden";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [rbw];

    xdg.configFile."rbw/config.json".text = ''
      {
        "email" : "mario.liguori.056@gmail.com",
        "pinentry" : "${pkgs.pinentry-gnome}/bin/pinentry-gnome3"
      }
    '';
  };
}
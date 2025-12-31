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
    programs.rbw = {
      enable = true;
      settings = {
        email = "mario.liguori.056@gmail.com";
        pinentry =
          if pkgs.stdenv.isDarwin
          then pkgs.pinentry_mac
          else pkgs.pinentry-gnome3;
      };
    };
  };
}

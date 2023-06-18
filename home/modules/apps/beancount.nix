{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.apps.beancount;
in {
  options.mario.modules.apps.beancount = {
    enable = mkEnableOption "enable beancount for finances";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      beancount
      fava
    ];
  };
}

{
  pkgs,
  config,
  lib,
  options,
  ...
}:
with lib; let
  cfg = config.home.modules.desktop.apps.autorandr;
  aocUW = "00ffffffffffff0005e302341d240000261e0104b55021783f3ec5ad4f47a326125054bfef00d1c0b3009500818081c0316845686168e77c70a0d0a0295030203a001d4e3100001a4ed470a0d0a0465030403a001d4e3100001c000000fc005533344732473452330a202020000000fd003090dcdc50010a202020202020028102032cf14e0103051404131f12021190595a5c230907078301000065030c001000e305e301e6060701535300d8590060a3382840a0103a101d4e3100001af4b000a0a0384d4030203a001d4e3100001a023a801871382d40582c45001d4e3100001eef51b87062a0355080b83a001d4e3100001c0000000000000000000000de7012790000030128cffe00046f0d9f002f003f009f05450002800900b92d01046f0d9f002f003f009f05310002800900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000a790";
in {
  options.home.modules.desktop.apps.autorandr = {
    enable = mkEnableOption "autorandr configuration (xorg sucks)";
  };

  config = mkIf cfg.enable {
    programs.autorandr = {
      enable = true;

      profiles = {
        home = {
          fingerprint.DisplayPort-0 = aocUW;

          config.DisplayPort-0 = {
            enable = true;
            primary = true;
            crtc = 0;
            position = "0x0";
            mode = "3440x1440";
            rate = "144.00";
          };
        };
      };
    };
  };
}

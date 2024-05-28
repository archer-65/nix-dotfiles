{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.dev.tex;
in {
  options.mario.modules.dev.tex = {
    enable = mkEnableOption "texlive";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      texliveFull
      python311Packages.pygments
      inkscape
    ];
  };
}

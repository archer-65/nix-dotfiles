{
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
      (texliveFull.withPackages (ps: with ps; [fontawesome6 source-sans]))
      python314Packages.pygments
      inkscape
    ];
  };
}

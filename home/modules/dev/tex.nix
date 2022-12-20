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
      texlive.combined.scheme-full
      python310Packages.pygments
    ];
  };
}

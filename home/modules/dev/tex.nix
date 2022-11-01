{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.home.modules.dev.tex;
in {
  options.home.modules.dev.tex = {
    enable = mkEnableOption "texlive";
  };

  config = mkIf cfg.enable {
    home.packages = [pkgs.texlive.combined.scheme-full];
  };
}

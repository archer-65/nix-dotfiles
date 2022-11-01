{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.user-modules.dev.tex;
in {
  options.user-modules.dev.tex = {
    enable = mkEnableOption "texlive";
  };

  config = mkIf cfg.enable {
    home.packages = [pkgs.texlive.combined.scheme-full];
  };
}

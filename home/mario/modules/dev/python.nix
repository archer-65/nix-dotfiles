{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.dev.python;
in {
  options.mario.modules.dev.python = {
    enable = mkEnableOption "python language support and language server";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      python312Packages.python-lsp-server
      python312
    ];
  };
}

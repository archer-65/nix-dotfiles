{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.dev.yaml;
in {
  options.mario.modules.dev.yaml = {
    enable = mkEnableOption "yaml language extra tools and language server";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      yaml-language-server
      yamllint
      yamlfmt
      actionlint # # github actions linter
    ];
  };
}

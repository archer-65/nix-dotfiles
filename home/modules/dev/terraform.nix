{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.home.modules.dev.terraform;
in {
  options.home.modules.dev.terraform = {
    enable = mkEnableOption "terraform and official language server";
  };

  config =
    mkIf cfg.enable {home.packages = with pkgs; [terraform terraform-ls];};
}

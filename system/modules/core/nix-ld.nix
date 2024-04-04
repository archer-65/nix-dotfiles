{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.core.nix-ld;
in {
  options.system.modules.core.nix-ld = with types; {
    enable = mkEnableOption "Whether or not to enable nix-ld.";
  };

  config = mkIf cfg.enable {
    programs.nix-ld.enable = true;
    programs.nix-ld.package = pkgs.inputs.nix-ld-rs.nix-ld-rs;
  };
}

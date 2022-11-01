{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.home.modules.editors.neovim;
in {
  options.home.modules.editors.neovim = {
    enable = mkEnableOption "neovim";
  };

  config = mkIf cfg.enable {programs.neovim = {enable = true;};};
}

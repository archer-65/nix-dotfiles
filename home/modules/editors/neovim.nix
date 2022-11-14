{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.editors.neovim;
in {
  options.mario.modules.editors.neovim = {
    enable = mkEnableOption "neovim";
  };

  config = mkIf cfg.enable {programs.neovim = {enable = true;};};
}

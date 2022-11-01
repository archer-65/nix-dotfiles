{ config, lib, ... }:

with lib;
let cfg = config.user-modules.editors.neovim;
in {
  options.user-modules.editors.neovim = {
    enable = mkEnableOption "neovim";
  };

  config = mkIf cfg.enable { programs.neovim = { enable = true; }; };
}

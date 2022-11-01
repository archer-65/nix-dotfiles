{ config, lib, ... }:

with lib;
let cfg = config.user-modules.editors.neovim;
in {
  options.user-modules.editors.neovim = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable { programs.neovim = { enable = true; }; };
}

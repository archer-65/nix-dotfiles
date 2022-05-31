{ config, lib, pkgs, inputs, ... }:

with lib;
let cfg = config.user-modules.editors.neovim;
in {
  options.user-modules.editors.neovim = {
    enable = _.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    
    programs.neovim = {
      enable = true;
    };
  };
}
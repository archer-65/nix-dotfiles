{ config, lib, pkgs, inputs, ... }:

with lib;
let cfg = config.modules.editors.vscode;
in {
  options.user-modules.editors.vscode = {
    enable = _.mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    programs.vscode = {
      enable = true;
      extensions = with pkgs.vscode-extensions; [
        jnoortheen.nix-ide
        mskelton.one-dark-theme
      ];
    };
  };
}
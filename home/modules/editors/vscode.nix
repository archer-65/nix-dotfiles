_:
{ config, lib, pkgs, inputs, ... }:

with lib;
let cfg = config.user-modules.editors.vscode;
in {
  options.user-modules.editors.vscode = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
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

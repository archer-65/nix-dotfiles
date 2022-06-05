_:
{ config, lib, pkgs, ... }:

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

      # Commented atm, cause of internal sync of vscode 
      # and necessity ok out of store symlinks
      # extensions = with pkgs.vscode-extensions; [
      #   jnoortheen.nix-ide
      #   mskelton.one-dark-theme
      # ];
    };
  };
}
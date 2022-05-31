{ config, lib, ... }:

{
  lib._.user-modules = {
    editors = {
      emacs = {
        enable = true;
        daemon.enable = true;
      };

      vscode.enable = true;

      neovim.enable = true;
    };
  };
}
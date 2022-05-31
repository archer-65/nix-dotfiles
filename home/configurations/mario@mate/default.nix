{ config, lib, ... }:
{
  user-modules = {
    editors = {
      emacs = {
        enable = true;
        daemon.enable = true;
      };

      vscode.enable = true;
    };
  };
}
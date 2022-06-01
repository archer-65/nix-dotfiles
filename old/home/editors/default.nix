{ config, pkgs, lib, inputs, ... }:
let emacsModule = import ./emacs { inherit pkgs inputs; };
in { imports = [ ./vscode emacsModule ]; }

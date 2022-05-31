# Mate, main laptop
{ config, lib, ... }:

with lib.my;
{
  lib.my.modules = {
    hardware = {
      audio.enable = true;
      bluetooth.enable = true;
      monitoring.enable = true;
    };

    services = {
      ssh.enable = true;
      flatpak.enable = true;
    };

    core.boot.splashBoot.enable = true;
    cachix = true;
  };
}
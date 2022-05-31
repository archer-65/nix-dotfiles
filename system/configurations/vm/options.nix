# VM, just for fun
{ config, lib, ... }:

with lib.my;
{
  modules = {
    hardware = {
      audio.enable = true;
    };

    services = {
      ssh.enable = true;
    };

    cachix = true;
  };
}
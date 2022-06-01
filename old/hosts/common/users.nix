{ lib, config, pkgs, ... }:

{
  # Users configuration
  users.users.mario = {
    isNormalUser = true;
    extraGroups =
      [ "wheel" "video" "audio" "networkmanager" "kvm" "libvirtd" "plex" ];
    shell = pkgs.bash;
  };
}

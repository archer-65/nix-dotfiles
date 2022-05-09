{ lib, config, pkgs, user, ... }:

{
  # Users configuration
  users.users.${user} = {
    isNormalUser = true;
    extraGroups = [ "wheel" "video" "audio" "networkmanager" "kvm" "libvirtd" "plex" ];
    shell = pkgs.bash;
  };
}
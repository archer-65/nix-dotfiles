{ pkgs, inputs, lib, config, ... }:

{
  # System-wide packages
  environment = {
    systemPackages = with pkgs; [
      killall
      procps
      wget
      git
      vim
    ];
  };
}
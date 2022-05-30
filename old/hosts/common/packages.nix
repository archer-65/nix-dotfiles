{ pkgs, inputs, lib, config, ... }:

{
  # System-wide packages
  environment = {
    systemPackages = with pkgs; [
      alacritty
      cachix
      killall
      git
      lm_sensors
      smartmontools
      procps
      wget
      firefox
      vim
      emacs
    ];
  };
}

{ pkgs, inputs, lib, config, ... }:

{
  # System-wide packages
  environment = {
    systemPackages = with pkgs; [
      alacritty
      killall
      git
      lm_sensors
      smartmontools
      wget
      firefox
      vim
      emacs
    ];
  };
}

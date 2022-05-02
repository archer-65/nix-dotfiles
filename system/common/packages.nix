{ pkgs, inputs, lib, config, ... }:

{
  # System-wide packages
  environment = {
    systemPackages = with pkgs; [
      alacritty
      killall
      git
      wget
      firefox
      vim
      emacs
    ];
  };
}
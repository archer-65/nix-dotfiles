{ config, lib, pkgs, ... }:

{
  programs.dconf.enable = true;
     
  services = {
    gnome.gnome-keyring.enable = true;

    dbus = {
      enable = true;
      packages = [ pkgs.dconf ];
    };

    xserver = {
      enable = true;

      layout = "us";
      xkbVariant = "intl";

      libinput = {
        enable = true;
      };

      displayManager = {
        sddm.enable = true;
	      sddm.theme = "${(pkgs.fetchFromGitHub {
	        owner = "3ximus";
	        repo = "abstractdark-sddm-theme";
	        rev = "master";
	        sha256 = "XmhTVs/1Hzrs+FBRbFEOSIFOrRp0VTPwIJmSa2EgIeo=";
	      })}
	      #themeIni = [
        #  {section = 'General'; key = 'background'; value = ../../commodore.jpg; };
	      #]
	      ";
        defaultSession = "none+qtile";
      };

      desktopManager.wallpaper.mode = "fill";

      windowManager.qtile = {
        enable = true;
      };
    };
  };
}

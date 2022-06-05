{ config, pkgs, ... }: {
  user-modules = {
    credentials = {
      gpg.enable = true;
      mail-defaults.enable = true;
    };

    desktop = {
      apps.rofi.enable = true;

      media.documents = {
        enable = true;
        pdf.enable = true;
        pdf.enablePlus = true;
      };

      term.alacritty.enable = true;
    };

    dev = { nix.enable = true; };

    editors = {
      emacs = {
        enable = true;
        daemon.enable = true;
      };

      vscode.enable = true;
    };

    shell = {
      bash.enable = true;
      extensions.enable = true;
      starship.enable = true;

      direnv.enable = true;

      git-defaults.enable = true;
    };

    themes = {
      active = "materia";

      font = {
        name = "Roboto";
        size = 16;
      };
    };
  };

  xdg.userDirs = {
    enable = true;
    createDirectories = true;
    desktop = "${config.home.homeDirectory}/desktop";
    documents = "${config.home.homeDirectory}/docs";
    download = "${config.home.homeDirectory}/dl";
    music = "${config.home.homeDirectory}/music";
    pictures = "${config.home.homeDirectory}/pics";
    publicShare = "${config.home.homeDirectory}/public";
    templates = "${config.home.homeDirectory}/templates";
    videos = "${config.home.homeDirectory}/videos";
  };

  xdg.configFile."rbw/config.json".text = ''
    {
      "email" : "mariogt2009@live.it",
      "pinentry" : "${pkgs.pinentry-gnome}/bin/pinentry-gnome3"
    }
  '';
}

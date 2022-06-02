{ config, lib, ... }: {
  user-modules = {
    editors = {
      emacs = {
        enable = true;
        daemon.enable = true;
      };

      vscode.enable = true;
    };

    shell = {
      bash.enable = true;
      starship.enable = true;

      git = {
        enable = true;
        email = "mariogt2009@live.it";
        user = "archer-65";
      };

      gpg.enable = true;
    };

    theme.gtk = {
      active = "materia";

      font = {
        name = "VictorMono Nerd Font";
        size = 12;
      };
    };
   
    desktop = {
      term.alacritty.enable = true;
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
}

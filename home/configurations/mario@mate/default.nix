{ config, pkgs, ... }: {
  imports = [ ../mail.nix ];

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
      extensions.enable = true;
      starship.enable = true;

      direnv.enable = true;

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

    dev = { nix.enable = true; };

    desktop = {
      media.documents = {
        enable = true;
        pdf.enable = true;
      };
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

  xdg.configFile."rbw/config.json".text = ''
    {
      "email" : "mariogt2009@live.it",
      "pinentry" : "${pkgs.pinentry-gtk2}/bin/pinentry-gtk-2"
    }
  '';
}

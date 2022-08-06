{ config, pkgs, ... }: {
  user-modules = {
    credentials = {
      gpg.enable = true;
      mail-defaults.enable = true;
    };

    desktop = {
      apps = {
        rofi.enable = true;
        discord.enable = true;
        teams.enable = true;
      };

      browsers = { firefox.enable = true; };

      media.documents = {
        enable = true;
        pdf.enable = true;
      };

      xorg = {
        enable = true;
        qtile.enable = true;
      };

      # wayland = {
      #   enable = true;
      #   sway.enable = true;
      # };

      term.alacritty.enable = true;
    };

    dev = {
      nix.enable = true;
      tex.enable = true;
    };

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
        size = 12;
      };

      font.term = {
        name = "VictorMono Nerd Font";
        size = 12;
      };

      font.alt = {
        name = "Fira Code";
        size = 12;
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
    videos = "${config.home.homeDirectory}/videos";
    publicShare = "${config.home.homeDirectory}";
    templates = "${config.home.homeDirectory}";

    extraConfig = {
      XDG_PROJECTS_DIR = "${config.home.homeDirectory}/projects";
    };
  };

  xdg.configFile."rbw/config.json".text = ''
    {
      "email" : "mario.liguori.056@gmail.com",
      "pinentry" : "${pkgs.pinentry-gnome}/bin/pinentry-gnome3"
    }
  '';
}

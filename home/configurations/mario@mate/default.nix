{
  config,
  ...
}: {
  mario.modules = {
    credentials = {
      gpg = {
        enable = true;
        signing.enable = true;
        signing.key = "mario.liguori.056@gmail.com";
      };
      mail-defaults.enable = true;
      bitwarden.enable = true;
    };

    desktop = {
      apps = {
        rofi.enable = true;
        discord.enable = true;
        teams.enable = true;
      };

      browsers = {firefox.enable = true;};

      media = {
        documents = {
          zathura.enable = true;
        };

        videos.enable = true;
      };

      # xorg = {
      #   enable = true;
      #   wm = "qtile";
      # };

      wayland = {
        enable = true;
        wm = ["sway" "hyprland"];
      };

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
      active = "onedark";

      font = {
        name = "Roboto";
        size = 12;
      };

      font.term = {
        name = "VictorMono Nerd Font";
        size = 14;
      };

      font.alt = {
        name = "Fira Code";
        size = 12;
      };

      bar = {
        font = {
          name = "Iosevka Nerd Font";
          size = 14;
        };

        battery = "BAT1";
      };
    };
  };

  xdg.userDirs = {
    enable = true;
    createDirectories = true;

    # These are useless to me
    desktop = null;
    publicShare = null;
    templates = null;

    documents = "${config.home.homeDirectory}/docs";
    download = "${config.home.homeDirectory}/dl";
    music = "${config.home.homeDirectory}/music";
    pictures = "${config.home.homeDirectory}/pics";
    videos = "${config.home.homeDirectory}/videos";

    extraConfig = {
      XDG_PROJECTS_DIR = "${config.home.homeDirectory}/projects";
      XDG_GAMES_DIR = "${config.home.homeDirectory}/games";
      XDG_MAILS_DIR = "${config.home.homeDirectory}/mails";
    };
  };

  services = {
    keybase.enable = true;
  };
}

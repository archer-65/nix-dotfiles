{
  config,
  ...
}: {
  mario.modules = {
    credentials = {
      gpg.enable = true;
      mail-defaults.enable = true;
      bitwarden.enable = true;
    };

    apps = {
      rofi.enable = true;
      discord.enable = true;
      teams.enable = true;
    };

    browsers = {firefox.enable = true;};

    gaming.emulators = {switch.enable = true;};

    media = {
      documents = {
        zathura.enable = true;
        okular.enable = true;
      };

      videos.enable = true;
    };

    wayland = {
      enable = true;
      wm = ["sway" "hyprland"];
    };

    term.alacritty.enable = true;

    dev = {
      cc.enable = true;
      nix.enable = true;
      java.enable = true;
      js.enable = true;
      python.enable = true;
      rust.enable = true;
      tex.enable = true;
      terraform.enable = true;
    };

    editors = {
      emacs = {
        enable = true;
        daemon.enable = true;
        telega.enable = true;
      };

      # android.enable = true;
      intellij.enable = true;

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
      active = "modus";
      darkTheme = true;

      ui.font = {
        name = "Roboto";
        size = 16;
      };

      term.font = {
        name = "VictorMono Nerd Font";
        size = 18;
      };

      ui-alt.font = {
        name = "Fira Code";
        size = 14;
      };

      bar = {
        font = {
          name = "Iosevka Nerd Font";
          size = 20;
        };
      };

      cursor.size = 24;
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

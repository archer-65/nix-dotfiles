{
  pkgs,
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

    term = {
      font = {
        family = "VictorMono Nerd Font";
        package = pkgs.nerdfonts.override {fonts = ["VictorMono"];};
        size = 18;
      };
      alacritty.enable = true;
    };

    dev = {
      cc.enable = true;
      nix.enable = true;
      java.enable = true;
      js.enable = true;
      kube.enable = true;
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

      git.enable = true;
    };

    themes = {
      active = "modus";
      darkTheme = true;

      font.regular = {
        family = "Roboto";
        package = pkgs.roboto;
        size = 16;
      };

      font.monospace = {
        family = "FiraCode Nerd Font";
        package = pkgs.nerdfonts.override {fonts = ["FiraCode"];};
        size = 14;
      };

      bar = {
        font = {
          family = "Iosevka Nerd Font";
          package = pkgs.nerdfonts.override {fonts = ["Iosevka"];};
          size = 20;
        };
      };

      cursor.size = 24;
    };
  };
}

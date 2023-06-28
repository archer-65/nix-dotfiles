{pkgs, ...}: {
  mario.modules = {
    credentials = {
      gpg.enable = true;
      mail-defaults.enable = true;
      bitwarden.enable = true;
    };

    apps = {
      beancount.enable = true;
      rofi.enable = true;
      discord.enable = true;
      teams.enable = true;
    };

    browsers = {firefox.enable = true;};

    gaming.emulators = {switch.enable = true;};

    media = {
      documents = {
        zathura.enable = true;
      };

      videos.enable = true;
    };

    wayland = {
      enable = true;
      wm = ["sway" "hyprland"];
      waybar = {
        temperature = "/sys/class/hwmon/hwmon2/temp4_input";
      };
    };

    term = {
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
        telega.enable = false;
      };

      android.enable = false;
      intellij.enable = false;

      vscode.enable = true;
    };

    shell = {
      bash.enable = true;
      zsh.enable = true;
      extensions.enable = true;
      starship.enable = true;
      tmux.enable = true;

      direnv.enable = true;

      git.enable = true;
    };

    themes = {
      active = "modus";
      darkTheme = true;

      cursor.size = 24;

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

      font.term = {
        family = "VictorMono Nerd Font";
        package = pkgs.nerdfonts.override {fonts = ["VictorMono"];};
        size = 18;
      };

      font.bar = {
        family = "Iosevka Nerd Font";
        package = pkgs.nerdfonts.override {fonts = ["Iosevka"];};
        size = 20;
      };
    };
  };
}

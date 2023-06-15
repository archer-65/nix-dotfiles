{
  pkgs,
  config,
  ...
}: {
  imports = [ ./common.nix ];

  mario.modules = {
    credentials = {
      gpg.enable = true;
      bitwarden.enable = true;
    };

    apps = {
      rofi.enable = true;
      teams.enable = true;
    };

    browsers = {firefox.enable = true;};


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
        temperature = "/sys/class/hwmon/hwmon0/temp1_input";
      };
    };

    term = {
      alacritty.enable = true;
    };

    dev = {
      nix.enable = true;
      kube.enable = true;
      python.enable = true;
      tex.enable = true;
      terraform.enable = true;
    };

    editors = {
      emacs = {
        enable = true;
        daemon.enable = true;
        telega.enable = false;
      };

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
        size = 12;
      };

      font.monospace = {
        family = "FiraCode Nerd Font";
        package = pkgs.stable.nerdfonts.override {fonts = ["FiraCode"];};
        size = 12;
      };

      font.term = {
        family = "VictorMono Nerd Font";
        package = pkgs.stable.nerdfonts.override {fonts = ["VictorMono"];};
        size = 14;
      };

      font.bar = {
        family = "Iosevka Nerd Font";
        package = pkgs.stable.nerdfonts.override {fonts = ["Iosevka"];};
        size = 16;
      };
    };
  };
}

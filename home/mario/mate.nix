{
  pkgs,
  config,
  ...
}: {
  imports = [ ./common.nix ];

  mario.modules = {
    credentials = {
      gpg.enable = true;
      mail-defaults.enable = false;
      bitwarden.enable = true;
    };

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

    wayland = {
      enable = true;
      wm = ["sway" "hyprland"];
    };

    term = {
      font = {
        family = "VictorMono Nerd Font";
        package = pkgs.stable.nerdfonts.override {fonts = ["VictorMono"];};
        size = 14;
      };
      alacritty.enable = true;
    };

    dev = {
      nix.enable = true;
      kube.enable = true;
      tex.enable = true;
      terraform.enable = true;
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

      bar = {
        font = {
          family = "Iosevka Nerd Font";
          package = pkgs.stable.nerdfonts.override {fonts = ["Iosevka"];};
          size = 16;
        };

        battery = "BAT1";
        temperature = "/sys/class/hwmon/hwmon3/temp1_input";
      };
    };
  };
}

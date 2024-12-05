{pkgs, ...}: {
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
      waybar = {
        battery = "BAT1";
        temperature = "/sys/class/hwmon/hwmon3/temp1_input";
      };
    };

    term = {
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
        package = pkgs.nerd-fonts.fira-code;
        size = 12;
      };

      font.term = {
        family = "VictorMono Nerd Font";
        package = pkgs.nerd-fonts.victor-mono;
        size = 14;
      };

      font.bar = {
        family = "Iosevka Nerd Font";
        package = pkgs.nerd-fonts.iosevka;
        size = 16;
      };
    };
  };
}

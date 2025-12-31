{pkgs, ...}: {
  home.packages = with pkgs; [coreutils-prefixed kiro-cli];

  mario.modules = {
    credentials = {
      gpg.enable = true;
      bitwarden.enable = true;
    };

    browsers = {firefox.enable = true;};

    term = {
      alacritty.enable = true;
    };

    dev = {
      nix.enable = true;
      kube.enable = true;
      python.enable = true;
      terraform.enable = true;
      tex.enable = true;
    };

    editors = {
      emacs = {
        enable = true;
        # NOTE: Root cause https://www.reddit.com/r/emacs/comments/1heyuq4/comment/m28c73o
        daemon.enable = false;
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

      awscli.enable = true;
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
        package = pkgs.nerd-fonts.fira-code;
        size = 14;
      };

      font.term = {
        family = "VictorMono Nerd Font";
        package = pkgs.nerd-fonts.victor-mono;
        size = 18;
      };

      font.bar = {
        family = "Iosevka Nerd Font";
        package = pkgs.nerd-fonts.iosevka;
        size = 20;
      };
    };
  };
}

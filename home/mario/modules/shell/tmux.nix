{
  pkgs,
  options,
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.mario.modules.shell.tmux;
in {
  options.mario.modules.shell.tmux = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    programs.tmux = {
      enable = true;
      prefix = "C-Space";
      newSession = true;
      baseIndex = 1;
      clock24 = true;
      historyLimit = 8000;
      escapeTime = 20;
      terminal = "xterm-256color";
      sensibleOnTop = false;
      mouse = true;

      extraConfig = ''
        unbind %
        bind | split-window -h

        unbind '"'
        bind - split-window -v

        unbind R
        bind R source-file ~/.config/tmux/tmux.conf

        bind -r j resize-pane -D 5
        bind -r k resize-pane -U 5
        bind -r l resize-pane -R 5
        bind -r h resize-pane -L 5

        bind -r m resize-pane -Z
      '';
    };
  };
}

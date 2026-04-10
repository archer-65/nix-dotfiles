{
  config,
  lib,
  pkgs,
  outputs,
  ...
}:
with lib; let
  cfg = config.system.modules.graphical.greetd;
in {
  options.system.modules.graphical.greetd = {
    enable = mkEnableOption "greetd configuration";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      # TODO: change theme!
      (catppuccin-gtk.override {
        accents = ["mauve"];
        size = "compact";
        variant = "mocha";
      })
    ];

    # FIXME: greetd + sway seems borked. Find a workaround or remove the entire config.
    # tuigreet works great, but has to be tuned "graphically" if you care about that.
    services.greetd = {
      settings = {
        default_session = {
          # command = "${config.programs.sway.package}/bin/sway --config ${greetd-sway-config}";
          command = "${pkgs.tuigreet}/bin/tuigreet --time --remember";
          user = "greeter";
        };
      };
    };

    # I've had permission issues on `/var/empty/.cache` lately with greetd. Give him a home.
    users.users.greeter = {
      home = "/var/lib/greeter";
      createHome = true;
    };

    programs.regreet = {
      enable = true;
      settings = {
        theme = {
          name = "Adwaita";
          package = pkgs.gnome-themes-extra;
        };
        iconTheme = {
          name = "Adwaita";
          package = pkgs.adwaita-icon-theme;
        };
        cursorTheme = {
          name = "Bibata-Modern-Classic";
          package = pkgs.bibata-cursors;
        };
        font = {
          name = "Roboto";
          size = 16;
          package = pkgs.roboto;
        };
        background = {
          path = outputs.wallpapers.nixos-dark.src;
          fit = "Cover";
        };
        GTK = {
          application_prefer_dark_theme = lib.mkDefault true;
        };
      };
    };
  };
}

_:
{ options, config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.desktop.xorg;
in {
  options.modules.desktop.xorg = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    services.xserver.enable = true;

    services.xserver.displayManager = {
      #lightdm.enable = true;
      sddm = {
        enable = true;

        theme = "${(pkgs.fetchFromGitHub {
          owner = "Kangie";
          repo = "sddm-sugar-candy";
          rev = "master";
          sha256 = "sha256-p2d7I0UBP63baW/q9MexYJQcqSmZ0L5rkwK3n66gmqM=";
        })}
        ";
      };

      defaultSession = "none+qtile";
    };

    environment.systemPackages = with pkgs.libsForQt5.qt5; [
      qtgraphicaleffects
      qtsvg
      qtquickcontrols2
    ];

    services.xserver.windowManager = { qtile.enable = true; };

    services.xserver = {
      layout = "us";
      xkbVariant = "altgr-intl";
      xkbOptions = "ctrl:nocaps";
    };

    services.xserver.libinput.enable = true;
  };
}

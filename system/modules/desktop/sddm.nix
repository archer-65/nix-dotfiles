_:
{ options, config, lib, pkgs, ... }:

with lib;

let cfg = config.modules.desktop.sddm;
in {
  options.modules.desktop.sddm = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    services.xserver.displayManager = {
      sddm = {
        enable = true;

        theme = "${(pkgs.fetchFromGitHub {
          owner = "Kangie";
          repo = "sddm-sugar-candy";
          rev = "master";
          sha256 = "sha256-p2d7I0UBP63baW/q9MexYJQcqSmZ0L5rkwK3n66gmqM=";
        })}";
      };

      defaultSession = "none+qtile";
    };

    environment.systemPackages = with pkgs.libsForQt5.qt5; [
      qtgraphicaleffects
      qtsvg
      qtquickcontrols2
    ];
  };
}

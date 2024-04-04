{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.dev.kube;
in {
  options.mario.modules.dev.kube = {
    enable = mkEnableOption "kubernetes tools";
    krew = {
      enable = mkEnableOption "enable krew for kubectl plugins";
      package = mkPackageOption pkgs "krew" {};
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home.packages = with pkgs; [
        kubectl
        kind
        minikube
        kubernetes-helm
      ];

      mario.modules.dev.kube.krew.enable = lib.mkDefault true;
    })

    (mkIf cfg.krew.enable {
      assertions = [
        {
          assertion = cfg.enable;
          message = "You have to set mario.modules.dev.kube.enable to true!";
        }
      ];

      home.packages = [
        cfg.krew.package
      ];

      home.sessionVariables = {
        KREW_ROOT = "$XDG_DATA_HOME/krew";
      };

      home.sessionPath = [
        "\${KREW_ROOT:-$HOME/.krew}/bin"
      ];
    })
  ];
}

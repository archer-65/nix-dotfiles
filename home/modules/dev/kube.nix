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
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      kubectl
      kind
      minikube
    ];
  };
}

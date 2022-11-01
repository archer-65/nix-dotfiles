{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.shell.bash;
in {
  options.system.modules.shell.bash = {
    enable = mkEnableOption "bash system-wide completion and additions";
  };

  config = mkIf cfg.enable {
    programs.bash = {
      enableCompletion = true;
      enableLsColors = true;
    };

    environment.systemPackages = with pkgs; [
      bash-completion
      nix-bash-completions
    ];
  };
}

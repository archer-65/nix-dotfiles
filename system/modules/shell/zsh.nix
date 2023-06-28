{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.modules.shell.zsh;
in {
  options.system.modules.shell.zsh = {
    enable = mkEnableOption "zsh system-wide completion and additions";
  };

  config = mkIf cfg.enable {
    programs.zsh = {
      enable = true;
      enableCompletion = true;
      autosuggestions.enable = true;
    };

    environment.systemPackages = with pkgs; [
      zsh-completions
      nix-zsh-completions
    ];

    environment.pathsToLink = ["/share/zsh"];
  };
}

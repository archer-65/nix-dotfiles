{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.shell.bash;
in {
  options.modules.shell.bash = {
    enable = mkOption {
      default = true;
      type = types.bool;
      example = true;
    };
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

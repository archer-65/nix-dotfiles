{ options, config, lib, pkgs, ... }:

with lib;
let cfg = config.user-modules.dev.cc;
in {
  options.user-modules.dev.cc = {
    enable = mkOption {
      default = false;
      type = types.bool;
      example = true;
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      gcc
      clang-tools
      #clang
      valgrind
      gdb
      cmake
      cmake-language-server
      gnumake
    ];
  };
}

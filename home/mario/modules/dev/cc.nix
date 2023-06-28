{
  options,
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.mario.modules.dev.cc;
in {
  options.mario.modules.dev.cc = {
    enable = mkEnableOption "c language support and language server";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      clang
      clang-tools
      cmake
      cmake-language-server
      valgrind
      gdb
      # gcc
      # gnumake
    ];
  };
}

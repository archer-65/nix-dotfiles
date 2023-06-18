{
  pkgs,
  config,
  lib,
  wallpapers,
  ...
}:
with lib; let
  archivePkgs = with pkgs; [
    zip
    unzip
    unrar
  ];
in {
  nix = {
    package = lib.mkDefault pkgs.nix;
    settings = {
      experimental-features = ["nix-command" "flakes" "repl-flake"];
      warn-dirty = false;
    };
  };

  systemd.user.startServices = "sd-switch";

  programs.home-manager.enable = true;

  home.file =
    lib.attrsets.concatMapAttrs
    (name: value: {
      ${name} = {
        target = "${config.xdg.userDirs.pictures}/walls/${name}.${value.ext}";
        source = value.src;
      };
    })
    wallpapers;

  home.packages = archivePkgs;

  xdg.userDirs = {
    enable = true;
    createDirectories = true;

    # These are useless to me
    desktop = null;
    publicShare = null;
    templates = null;

    documents = "${config.home.homeDirectory}/docs";
    download = "${config.home.homeDirectory}/dl";
    music = "${config.home.homeDirectory}/music";
    pictures = "${config.home.homeDirectory}/pics";
    videos = "${config.home.homeDirectory}/videos";

    extraConfig = {
      XDG_PROJECTS_DIR = "${config.home.homeDirectory}/projects";
      XDG_WORK_DIR = "${config.home.homeDirectory}/work";
      XDG_GAMES_DIR = "${config.home.homeDirectory}/games";
      XDG_MAILS_DIR = "${config.home.homeDirectory}/mails";
    };
  };

  services = {
    keybase.enable = true;
    kbfs.enable = true;
  };
}

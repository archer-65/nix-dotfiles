{
  pkgs,
  config,
  lib,
  outputs,
  ...
}: {
  nix = {
    package = lib.mkForce pkgs.nixVersions.latest;
    settings = {
      experimental-features = ["nix-command" "flakes"];
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
    outputs.wallpapers;

  xdg.userDirs = {
    enable =
      if pkgs.stdenv.isDarwin
      then false
      else true;
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
      XDG_GAMES_DIR = "${config.home.homeDirectory}/games";
      XDG_MAILS_DIR = "${config.home.homeDirectory}/mails";
    };
  };

  services = lib.optionalAttrs (pkgs.stdenv.isDarwin != true) {
    keybase.enable = true;
    kbfs = {
      enable = true;
      mountPoint = ".keybase";
    };
  };
}

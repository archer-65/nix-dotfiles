{ pkgs, ... }:

{
  picom = {
    enable = true;
    package = pkgs.picom.overrideAttrs (old: {
      src = pkgs.fetchFromGitHub {
      repo = "picom";
      owner = "ibhagwan";
      rev = "next-rebase";
      sha256 = "1hVFBGo4Ieke2T9PqMur1w4D0bz/L3FAvfujY9Zergw=";
      };
    });
  };
}
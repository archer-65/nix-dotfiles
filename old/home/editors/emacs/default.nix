{ inputs, ... }:
let 
  inherit (inputs) self;
  inherit (self) pkgs;
in {
  programs.emacs = {
    enable = true;
    package = pkgs.emacsPgtkNativeComp;
    #extraPackages = epkgs: [epkgs.vterm];
  };
}


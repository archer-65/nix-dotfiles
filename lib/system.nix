inputs:
let inherit (inputs.nixpkgs) lib;
in rec {
  supportedSystems = [ "x86_64-linux" ];

  genSystems = lib.genAttrs supportedSystems;

  nixpkgsFor = genSystems
    (system: overlays: import inputs.nixpkgs { inherit system overlays; });
}

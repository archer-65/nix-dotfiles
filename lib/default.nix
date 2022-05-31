# { inputs, lib, nixpkgs, ... }:

# let
#   inherit (lib) makeExtensible attrValues foldr;
#   inherit (modules) mapModules;

#   modules = import ./modules.nix {
#     inherit lib;
#     self.attrs = import ./attrs.nix { inherit lib; self = {}; };
#   };

#   mylib = makeExtensible (self:
#     with self; mapModules ./.
#       (file: import file { inherit self lib mixpkgs inputs; }));
# in
# mylib.extend
#   (self: super:
#     foldr (a: b: a // b) {} (attrValues super))
{ inputs, lib, pkgs, ... }:

lib.extend (lib: super:
  let
    inherit (builtins) attrNames map readDir;
    inherit (lib) filterAttrs foldr hasSuffix;

    importLib = file: import file { inherit inputs lib pkgs; };
    merge = foldr (a: b: a // b) {};
    importLibs = libs: merge (map importLib libs);

    isLib = name: type: type == "regular" && name != "default.nix" && hasSuffix ".nix" name;
    libPath = name: "${toString ./.}/${name}";
    libsInFolder = map libPath (attrNames (filterAttrs isLib (readDir ./.)));
  in {
    _ = importLibs libsInFolder;
  }
)
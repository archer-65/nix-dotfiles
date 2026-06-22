{
  lib,
  stdenvNoCC,
  fetchurl,
  ...
}: let
  version = "0.2.0";
in
  stdenvNoCC.mkDerivation {
    pname = "meridian-plugin-opencode-scrub";
    inherit version;

    src = fetchurl {
      url = "https://registry.npmjs.org/@rynfar/meridian-plugin-opencode-scrub/-/meridian-plugin-opencode-scrub-${version}.tgz";
      hash = "sha512-RaXNkIwPB2NmicdNvDmYLKbLz5KsnAlNt3XfuejstPpMZBbogxhi6JSsU3k6nHGj/yoYmbpLYMGD4Bv4v30Tvg==";
    };

    dontBuild = true;

    installPhase = ''
      runHook preInstall
      mkdir -p $out/lib
      cp -r dist $out/lib/
      runHook postInstall
    '';

    meta = {
      description = "Meridian plugin: strip OpenCode identifying fingerprints from the system prompt";
      homepage = "https://github.com/rynfar/meridian-plugin-opencode-scrub";
      license = lib.licenses.mit;
    };
  }

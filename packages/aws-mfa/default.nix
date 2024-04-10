{
  lib,
  pkgs,
  stdenv,
  makeWrapper,
  awscli2,
  coreutils,
  gnugrep,
  jq,
}:
with lib; let
  name = "aws-mfa";
in
  stdenv.mkDerivation {
    inherit name;
    src = ./aws-mfa.sh;

    nativeBuildInputs = [makeWrapper];

    dontUnpack = true;
    dontBuild = true;
    dontConfigure = true;

    installPhase = ''
      install -Dm 0755 $src $out/bin/${name}
      wrapProgram $out/bin/${name} --prefix PATH ':' \
        "${
        makeBinPath [
          awscli2
          coreutils
          gnugrep
          jq
        ]
      }"
    '';

    meta = {
      description = "Script to generate AWS MFA credentials with STS";
      platforms = platforms.all;
      mainProgram = "${name}";
    };
  }

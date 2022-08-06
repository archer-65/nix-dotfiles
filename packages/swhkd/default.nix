{ lib, fetchFromGitHub, rustPlatform }:

rustPlatform.buildRustPackage rec {
  pname = "swhkd";
  version = "2997d94b6dc6b460abc1131f17113e9e543c869e";

  src = fetchFromGitHub {
    owner = "waycrate";
    repo = "swhkd";
    rev = version;
    sha256 = "sha256-Mbh0Yx/v9l/kuXP2hUpEjs9orZhBI9ciytoxBUrUVkI=";
  };

  # cargoSha256 = "sha256-NAVqwYJA0+X0dFC3PBaW+QJxvJtSgl4Y/VNfNO3jnLA=";
  cargoLock.lockFile = ./Cargo.lock;

  meta = with lib; {
    description = "Sxhkd clone for Wayland";
    longDescription = ''
      Simple Wayland HotKey Daemon
      swhkd is a display protocol-independent hotkey daemon made in Rust. swhkd uses an easy-to-use configuration system inspired by sxhkd so you can easily add or remove hotkeys.
    '';
    homepage = "https://github.com/waycrate/swhkd";
    license = "BSD-2-Clause";
    platforms = with platforms; linux;
  };
}

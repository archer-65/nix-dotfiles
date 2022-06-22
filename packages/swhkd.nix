{ lib, fetchFromGitHub, rustPlatform }:

rustPlatform.buildRustPackage rec {
  pname = "swhkd";
  version = "d4d7f762826d69d8c9cf95f4d58b9e230fc76a2c";

  src = fetchFromGitHub {
    owner = "waycrate";
    repo = "swhkd";
    rev = version;
    sha256 = "sha256-cTLaw37WT0Yy9Wy2qHyaahCwmXSQk9oaCLtjXvxZPXA=";
  };

  cargoSha256 = "sha256-aWztHxoeRuFN4siuhMQtT7CWFknKGaWUCA2SOkg3jxk=";

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

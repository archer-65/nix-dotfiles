# Thanks to Matteo Joliveau.
# Original file: `https://github.com/https://github.com/MatteoJoliveau/nix-dotfiles/blob/master/nixpkgs/pkgs/rofi-rbw.nix`

{ pkgs, lib, pypkgs, fetchurl, }:

pypkgs.buildPythonPackage rec {
  pname = "rofi_rbw";
  version = "0.5.0";
  format = "wheel";

  src = fetchurl {
    url =
      "https://files.pythonhosted.org/packages/py3/r/rofi-rbw/${pname}-${version}-py3-none-any.whl";
    sha256 = "sha256-7TowxLxp4+4yGhcqMQa8po+uhnNbXShNEHJs6Vjwq+g=";
  };

  # We can't use it because the name of the package (rofi-rbw) is different than the name of the wheel (rofi_rbw)
  # src = pypkgs.fetchPypi {
  #   inherit pname version;
  #   format = "wheel";
  #   python = "py3";
  #   sha256 = "04bh4k0xvr89f4xh1mw47l8jayv8ck2c5gm48zai1j85z9xj7sh2";
  # };

  propagatedBuildInputs = with pkgs; [
    pypkgs.ConfigArgParse
    xdotool
    xclip
    wtype
    wl-clipboard
  ];
  buildInputs = with pkgs; [ rbw ];

  meta = with lib; {
    description = "A rofi frontend for Bitwarden";
    longDescription = ''
      A rofi frontend for Bitwarden
      Based on the alternative Bitwarden CLI rbw and inspired by rofi-pass, rbw-rofi is a simplistic password typer/copier using rofi.
    '';
    homepage = "https://github.com/fdw/rofi-rbw";
    license = "MIT";
    platforms = with platforms; linux;
    maintainers = [ maintainers.fdw ];
  };
}

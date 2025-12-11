{
  lib,
  stdenv,
  fetchurl,
  pkgs,
}:
stdenv.mkDerivation rec {
  pname = "kiro-cli";
  version = "1.21.0";

  src = fetchurl {
    inherit ((lib.importJSON ./sources.json).${stdenv.hostPlatform.system}) url sha256;
  };

  updateScript = ./update.sh;

  buildInputs = with pkgs;
    [
      stdenv.cc.cc.lib
      zlib
      openssl
    ]
    # Check for Wayland deps
    ++ lib.optionals pkgs.stdenv.isLinux [
      alsa-lib
      at-spi2-atk
      at-spi2-core
      atk
      cairo
      cups
      dbus
      expat
      fontconfig
      freetype
      gdk-pixbuf
      glib
      gtk3
      libdrm
      libnotify
      libsecret
      libuuid
      libxkbcommon
      mesa
      nspr
      nss
      pango
      systemd
      xorg.libX11
      xorg.libXcomposite
      xorg.libXdamage
      xorg.libXext
      xorg.libXfixes
      xorg.libXrandr
      xorg.libxcb
      xorg.libxshmfence
    ];

  nativeBuildInputs = with pkgs;
    lib.optionals pkgs.stdenv.isLinux
    [
      unzip
      autoPatchelfHook
    ]
    ++ lib.optionals pkgs.stdenv.isDarwin [
      undmg
    ];

  sourceRoot = ".";

  dontStrip = true;

  unpackPhase =
    if pkgs.stdenv.isDarwin
    then ''
      undmg $src
    ''
    else ''
      unzip $src
    '';

  installPhase =
    if pkgs.stdenv.isDarwin
    then ''
      mkdir -p $out/{bin,Applications}
      cp -r "Kiro CLI.app" $out/Applications/
      # Create symlink to the CLI binary
      ln -s "$out/Applications/Kiro CLI.app/Contents/MacOS/kiro-cli" $out/bin/kiro-cli
    ''
    else ''
      mkdir -p $out/bin

      # The zip contains a kirocli directory with binaries
      cd kirocli

      # Copy the main binaries
      cp kiro-cli $out/bin/
      cp kiro-cli-chat $out/bin/

      # Make them executable
      chmod +x $out/bin/kiro-cli
      chmod +x $out/bin/kiro-cli-chat

      # Copy any resources if they exist
      if [ -d resources ]; then
        mkdir -p $out/share/kiro-cli
        cp -r resources/* $out/share/kiro-cli/
      fi
    '';

  meta = with pkgs.lib; {
    description = "Kiro CLI - AI-powered coding assistant for the command line";
    homepage = "https://kiro.dev";
    license = licenses.unfree;
    platforms = ["x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin"];
    mainProgram = "kiro-cli";
  };
}

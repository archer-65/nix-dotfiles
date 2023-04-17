# With inputs as an argument it is possible to access to the flake itself. This could be helpful in future.
inputs: {
  # Overlays for personal pkgs (callPackage)
  additions = final: _: import ../packages {pkgs = final;};

  # Overlays for various pkgs (e.g. broken, not updated)
  modifications = final: prev: {
    waybar = prev.waybar.overrideAttrs (oldAttrs: {
      mesonFlags = oldAttrs.mesonFlags ++ ["-Dexperimental=true"];
    });

    lieer = prev.lieer.overrideAttrs(oldAttrs: {
      pname = "lieer";
      version = "1.3";

      src = final.fetchFromGitHub {
        owner = "gauteh";
        repo = "lieer";
        #rev = "v${version}";
        rev = "a4ab209c721de7146f4301b426bdb6482d687a85";
        sha256 = "sha256-BL5F7sJ818Ky33eJDR9Eh5/XayhddkBWxk1HkQxh0Qc=";
      };
 
      propagatedBuildInputs = with final.pkgs.python3Packages; [
        notmuch2
        oauth2client
        google-api-python-client
        tqdm
        setuptools
      ];
    });

    # notmuch-mailmover = inputs.notmuch-mailmover.packages.x86_64-linux.default;

    # tdlib = prev.tdlib.overrideAttrs (oldAttrs: {
    #   version = "1.8.8";
    #   src = final.fetchFromGitHub {
    #     owner = "tdlib";
    #     repo = "td";
    #     rev = "bbe37ee594d97f3c7820dd23ebcd9c9b8dac51a0";
    #     sha256 = "sha256-jLJglvq+7f+zCoanDRTFpUsH/M1Qf7PWJ1JjvmZsa24==";
    #   };
    # });
  };
}

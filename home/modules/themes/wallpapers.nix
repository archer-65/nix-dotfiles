let
  inherit (builtins) fetchurl;
in {
  wallpapers = {
    nerd-bedroom = let
      prefix = "wallhaven";
      id = "j3mmdy";
      ext = "jpg";
    in
      fetchurl rec {
        name = "${prefix}-${id}.${ext}";
        url = "https://w.wallhaven.cc/full/j3/${name}";
        sha256 = "1cfpc736bqr71ymwfib2z9cg4s1w05w3b0p6i4bdqnc84gb5mxjm";
      };

    messy-bedroom = let
      prefix = "wallhaven";
      id = "2e8v5x";
      ext = "png";
    in
      fetchurl rec {
        name = "${prefix}-${id}.${ext}";
        url = "https://w.wallhaven.cc/full/2e/${name}";
        sha256 = "0k48497yfh34sh7m2r6qwrg2qzgfjxc8dnzgzbkm8k89byhxn0k0";
      };

    girl-on-balcony = let
      prefix = "wallhaven";
      id = "95j8kw";
      ext = "jpg";
    in
      fetchurl rec {
        name = "${prefix}-${id}.${ext}";
        url = "https://w.wallhaven.cc/full/95/${name}";
        sha256 = "0az55dqb73mn4akdjrxv7xfvh5g35w06mrc2014zgrlk7ys2dlaq";
      };

    suzumiya-haruhi = let
      prefix = "wallhaven";
      id = "l3182l";
      ext = "jpg";
    in
      fetchurl rec {
        name = "${prefix}-${id}.${ext}";
        url = "https://w.wallhaven.cc/full/l3/${name}";
        sha256 = "0r1nnrv2gyghr572b6j6m2lp1akg3ws24ds7s7jjzcyghaw9l7dg";
      };

    nixos-blue = let
      prefix = "wallhaven";
      id = "e7djlk";
      ext = "png";
    in
      fetchurl rec {
        name = "${prefix}-${id}.${ext}";
        url = "https://w.wallhaven.cc/full/e7/${name}";
        sha256 = "1mwvnmflp0z1biyyhfz7mjn7i1nna94n7jyns3na2shbfkaq7i0h";
      };

    nixos-dark = let
      prefix = "wallhaven";
      id = "pkrqze";
      ext = "png";
    in
      fetchurl rec {
        name = "${prefix}-${id}.${ext}";
        url = "https://w.wallhaven.cc/full/pk/${name}";
        sha256 = "07zl1dlxqh9dav9pibnhr2x1llywwnyphmzcdqaby7dz5js184ly";
      };
  };
}

let
  inherit (builtins) fetchurl substring;

  wallhaven = {
    id,
    ext,
    sha256,
  }: {
    inherit ext;
    src = fetchurl {
      inherit sha256;
      url = "https://w.wallhaven.cc/full/${substring 0 2 id}/wallhaven-${id}.${ext}";
    };
  };
in {
  city-lights = wallhaven {
    id = "1pke89";
    ext = "jpg";
    sha256 = "0mkfkji3cz2sl6ipahq7ydzmag9jdx85g8nn5madrxdb36qfrhs8";
  };

  moonlight-car = wallhaven {
    id = "m3dqj8";
    ext = "jpg";
    sha256 = "1w6snp4ki0rx2897rhgmhm1gsr005b3ddc455zvy0bga00l7mwnl";
  };

  nerd-bedroom = wallhaven {
    id = "j3mmdy";
    ext = "jpg";
    sha256 = "1cfpc736bqr71ymwfib2z9cg4s1w05w3b0p6i4bdqnc84gb5mxjm";
  };

  messy-bedroom = wallhaven {
    id = "2e8v5x";
    ext = "png";
    sha256 = "0k48497yfh34sh7m2r6qwrg2qzgfjxc8dnzgzbkm8k89byhxn0k0";
  };

  girl-on-balcony = wallhaven {
    id = "95j8kw";
    ext = "jpg";
    sha256 = "0az55dqb73mn4akdjrxv7xfvh5g35w06mrc2014zgrlk7ys2dlaq";
  };

  suzumiya-haruhi = wallhaven {
    id = "l3182l";
    ext = "jpg";
    sha256 = "0r1nnrv2gyghr572b6j6m2lp1akg3ws24ds7s7jjzcyghaw9l7dg";
  };

  digital-flowers = wallhaven {
    id = "e7oxrr";
    ext = "jpg";
    sha256 = "1qkwzlsfgxk4yvyv2pagz8k035h385419dgc951xqqkzh1z77d7v";
  };

  nixos-blue = wallhaven {
    id = "e7djlk";
    ext = "png";
    sha256 = "1mwvnmflp0z1biyyhfz7mjn7i1nna94n7jyns3na2shbfkaq7i0h";
  };

  nixos-dark = wallhaven {
    id = "pkrqze";
    ext = "png";
    sha256 = "07zl1dlxqh9dav9pibnhr2x1llywwnyphmzcdqaby7dz5js184ly";
  };
}

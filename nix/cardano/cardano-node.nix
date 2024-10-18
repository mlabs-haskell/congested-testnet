{ inputs, self, ... }:
{
  perSystem = { system, inputs', pkgs, ... }:
    {
      packages.cardano-node = pkgs.stdenv.mkDerivation {
        pname = "cardano-node";
        version = "9.2.0";

        src = pkgs.fetchurl {
          url = "https://github.com/IntersectMBO/cardano-node/releases/download/9.2.0/cardano-node-9.2.0-linux.tar.gz";
          sha256 = "sha256-RAGXnssISG++ekXQPjXzC2JW6L6R/w9VEGwG0LPx5LQ=";
        };

        buildCommand = ''
          mkdir $out
          tar -C $out --strip-components=1 -xf $src
        '';
      };
    };
}

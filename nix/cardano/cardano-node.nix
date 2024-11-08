{ inputs, self, ... }:
{
  perSystem = { system, inputs', pkgs, ... }:
    {
      packages.cardano-node-1 = pkgs.stdenv.mkDerivation {
        pname = "cardano-node";
        version = "10.1.1";

        src = pkgs.fetchurl {
          url = "https://github.com/IntersectMBO/cardano-node/releases/download/10.1.1/cardano-node-10.1.1-linux.tar.gz";
          sha256 = "sha256-XH9T84KKZzpyGIUOV+vWGLC0rQZzZOQTHhO1qXoYgnI=";
        };

        buildCommand = ''
          mkdir $out
          tar -C $out --strip-components=1 -xf $src
        '';
      };
    };
}

{ inputs, self, ... }:
{
  perSystem = { system, inputs', pkgs, ... }:
    {
      packages.cardano-node = pkgs.stdenv.mkDerivation {
        pname = "cardano-node";
        # version = "10.1.1";
        version = "10.1.4";

        src = pkgs.fetchurl {
          # url = "https://github.com/IntersectMBO/cardano-node/releases/download/10.1.1/cardano-node-10.1.1-linux.tar.gz";
          # sha256 = "sha256-XH9T84KKZzpyGIUOV+vWGLC0rQZzZOQTHhO1qXoYgnI=";
          url = "https://github.com/IntersectMBO/cardano-node/releases/download/10.1.4/cardano-node-10.1.4-linux.tar.gz";
          sha256 = "sha256-r7gvMCWkwbhFzgjC64nI3IaS6DJvR45Oo3zh4gY/4/Y=";
        };

        buildCommand = ''
          mkdir $out
          tar -C $out --strip-components=1 -xf $src
        '';
      };
    };
}

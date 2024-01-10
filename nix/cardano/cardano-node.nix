{ inputs, self, ... }:
{
  perSystem = { system, inputs', pkgs, ... }:
    {
      packages.cardano-node = pkgs.stdenv.mkDerivation {
        pname = "cardano-node";
        version = "8.1.2";

        src = pkgs.fetchurl {
          url = "https://github.com/input-output-hk/cardano-node/releases/download/8.1.2/cardano-node-8.1.2-linux.tar.gz";
          sha256 = "sha256-NakRbNfUf1J9NICFOq+HMrfPHurPemdTC8pqf9aeUPo=";
        };

        buildCommand = ''
          mkdir $out
          tar -C $out --strip-components=1 -xf $src
          cd $out
          mkdir $out/bin
          ln -s $out/cardano-cli $out/bin/cardano-cli
          ln -s $out/cardano-node $out/bin/cardano-node
        '';
      };
    };
}

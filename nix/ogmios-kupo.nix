{ inputs, self, ... }:
{
  # ! kupo is taken from ctl overlays
  perSystem = { system, inputs', self', pkgs, ... }:
    {

      # here we override ogmios for a fresher version
      packages.ogmios = pkgs.stdenv.mkDerivation {
        pname = "ogmios";
        version = "6.11.0";
        buildInputs = [ pkgs.unzip ];

        src = pkgs.fetchurl {
          url = "https://github.com/CardanoSolutions/ogmios/releases/download/v6.11.0/ogmios-v6.11.0-x86_64-linux.zip";
          sha256 = "sha256-zrm46h/CThI8ZVLi4XBeo4q2a61zQD0ZQpRUiVO9798=";
        };

        buildCommand = ''
          mkdir $out
          unzip $src -d $out
          chmod -R +x $out/bin
        '';
      };

    };
}

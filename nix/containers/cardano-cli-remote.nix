{ inputs, ... }:
{

  perSystem =
    { pkgs
    , self'
    , system
    , ...
    }:
    let craneLib = inputs.crane.lib.${system};
    in
    {

      packages.cardano-cli-remote = craneLib.buildPackage {
        src = craneLib.cleanCargoSource (craneLib.path ../../cardano-cli-remote);
      };



      packages.cardano-cli-remote-container = 
        pkgs.writeShellApplication {
          name = "cardano-cli-remote-container";
          runtimeInputs = with pkgs; [ 
            coreutils
            self'.packages.cardano-node
            bashInteractive
          ];
          text = ''
            ${self'.packages.cardano-cli-remote}/bin/cardano-cli-remote
          '';
        };

    };
}

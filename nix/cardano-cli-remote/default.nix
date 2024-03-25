{ inputs, ... }:
{

  perSystem =
    {
    pkgs
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

    };
 }

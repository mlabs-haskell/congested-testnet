{ inputs, ... }:
{
  flake.nixosModules = {
    imports = [ ./desktop.nix ];
  };

  perSystem = { system, pkgs, self',inputs', ... }:
    {
      packages.vm = inputs.nixos-generators.nixosGenerate {
        inherit system;
        modules = [
          inputs.arion.nixosModules.arion
          ./vars.nix
          ./desktop.nix
          ./user.nix
          (import ./congested-testnet { inherit self'; })
        ];
        format = "vm";
      };
      packages.check =
      let
        pkgs' = pkgs // { congested.faucet = self'.packages.faucet; 
         congested.cardano-node = inputs'.cardano-node.legacyPackages.cardano-node;
         congested.testnet-conf = pkgs.runCommand "testnet-conf" {} '' 
            mkdir -p $out
            cp -r ${../../testnet-conf}/* $out/
        ''; 
        };
        arion-compose = pkgs.arion.build { modules = [ ./congested-testnet/arion-compose.nix ]; pkgs = pkgs'; };
      in
      pkgs.writeShellApplication {
        name = "start";
        runtimeInputs = [ pkgs.arion pkgs.docker ];
        text = ''
          #!/bin/sh
          jq < ${arion-compose}  
          ${pkgs.arion}/bin/arion --prebuilt-file ${arion-compose} down -v  
          ${pkgs.arion}/bin/arion --prebuilt-file ${arion-compose} up -d  --remove-orphans
          ${pkgs.arion}/bin/arion --prebuilt-file ${arion-compose} logs -f 
        '';
           # ls ${pkgs'.congested.testnet-conf}
    };
    };
}

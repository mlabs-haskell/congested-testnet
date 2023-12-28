{ inputs, ... }:
{
  flake.nixosModules = {
    imports = [ ./desktop.nix ];
  };

  perSystem = { system, pkgs, self', inputs', ... }:
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
      packages.arion = inputs'.arion.packages.arion;
      packages.check =
        let
          pkgs' = pkgs // {
            arion' = self'.packages.arion;
            congested.faucet = self'.packages.faucet;
            congested.cardano-node = inputs'.cardano-node.legacyPackages.cardano-node;
            congested.gen-testnet-conf = self'.packages.gen-testnet-conf;
          };
          arion-compose = pkgs'.arion'.build { modules = [ ./congested-testnet/arion-compose.nix ]; pkgs = pkgs'; };
        in
        pkgs.writeShellApplication {
          name = "start";
          runtimeInputs = [ pkgs'.arion' pkgs.yq];
          text = ''
            #!/bin/sh
            arion --prebuilt-file ${arion-compose} down -v  
            arion --prebuilt-file ${arion-compose} up -d --remove-orphans 
            arion --prebuilt-file ${arion-compose} logs testnet-config -f 
          '';
        };
    };
}

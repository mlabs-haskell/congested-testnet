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
            congested = {
              faucet = self'.packages.faucet;
              cardano-node = inputs'.cardano-node.legacyPackages.cardano-node;
              gen-testnet-conf = self'.packages.gen-testnet-conf;
              make-faucet-wallet = self'.packages.make-faucet-wallet;
              ogmios = self'.packages.ogmios;
              gen-wallet = self'.packages.gen-wallet;
            };
          };
          arion-compose = pkgs'.arion'.build { modules = [ ./congested-testnet/arion-compose.nix ]; pkgs = pkgs'; };
        in
        pkgs.writeShellApplication {
          name = "start";
          runtimeInputs = [ pkgs'.arion' ];
          text = ''
            #!/bin/sh
            # arion --prebuilt-file ${arion-compose} down -v 
            # arion --prebuilt-file ${arion-compose} up -d 
            # arion --prebuilt-file ${arion-compose} logs -f 
            arion --prebuilt-file ${arion-compose} down spammer 
            arion --prebuilt-file ${arion-compose} up -d spammer 
            arion --prebuilt-file ${arion-compose} logs -f spammer 
          '';
        };
    };
}

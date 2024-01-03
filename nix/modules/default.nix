{ self, inputs, ... }:
{
  flake.nixosModules = {
    imports = [ ./desktop.nix ];
  };

  flake.overlays.default = final: prev: {
    faucet = self.packages.${final.system}.faucet;
    cardano-node = inputs.cardano-node.legacyPackages.${final.system}.cardano-node;
    gen-testnet-conf = self.packages.${final.system}.gen-testnet-conf;
    ogmios = self.packages.${final.system}.ogmios;
  };
  
  flake.nixosConfigurations = {
    congested-testnet = inputs.nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
	inputs.arion.nixosModules.arion
        ./congested-testnet
	{ nixpkgs.overlays = [self.overlays.default];}
      ];
    };
  };
  
  perSystem = { system, pkgs, self', inputs', ... }:
    {
      packages.vm = (self.nixosConfigurations.congested-testnet.extendModules {
	modules = [ (import ./congested-testnet/vm.nix) ];
      }).config.system.build.vm;
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
              spammer = self'.packages.spammer;
            };
          };
          arion-compose = pkgs'.arion'.build { modules = [ ./congested-testnet/arion-compose.nix ]; pkgs = pkgs'; };
        in
        pkgs.writeShellApplication {
          name = "start";
          runtimeInputs = [ pkgs'.arion' ];
          text = ''
            #!/bin/sh
            arion --prebuilt-file ${arion-compose} down -v 
            arion --prebuilt-file ${arion-compose} up -d 
            arion --prebuilt-file ${arion-compose} logs -f spammer faucet
          '';
            # arion --prebuilt-file ${arion-compose} down spammer 
            # arion --prebuilt-file ${arion-compose} up -d spammer 
            # arion --prebuilt-file ${arion-compose} logs -f spammer 
        };
    };
}

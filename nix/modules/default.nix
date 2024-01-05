{ self, inputs, ... }:
{
  flake.nixosModules = {
    imports = [ ./desktop.nix ];
  };

  flake.overlays.default = final: prev: {
    faucet = self.packages.${final.system}.faucet;
    gen-testnet-conf = self.packages.${final.system}.gen-testnet-conf;
    ogmios-run = self.packages.${final.system}.ogmios-run;
    kupo-run = self.packages.${final.system}.kupo-run;
    gen-wallet = self.packages.${final.system}.gen-wallet;
    spammer = self.packages.${final.system}.spammer;
    make-faucet-wallet = self.packages.${final.system}.make-faucet-wallet;
    relay-node = self.packages.${final.system}.relay-node;
    spo-node = self.packages.${final.system}.spo-node;
    prometheus-run = self.packages.${final.system}.prometheus-run;

    podman = inputs.arion.inputs.nixpkgs.legacyPackages.${final.system}.podman;
    docker = inputs.arion.inputs.nixpkgs.legacyPackages.${final.system}.docker;
    docker-client = inputs.arion.inputs.nixpkgs.legacyPackages.${final.system}.docker-client;
    docker-compose = inputs.arion.inputs.nixpkgs.legacyPackages.${final.system}.docker-compose;
    arion = inputs.arion.inputs.nixpkgs.legacyPackages.${final.system}.arion;
  };

  flake.nixosConfigurations = {
    congested-testnet = inputs.nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        inputs.arion.nixosModules.arion
        ./congested-testnet
        { nixpkgs.overlays = [ self.overlays.default ]; }
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
            faucet = self'.packages.faucet;
            cardano-node = self'.packages.cardano-node;
            gen-testnet-conf = self'.packages.gen-testnet-conf;
            make-faucet-wallet = self'.packages.make-faucet-wallet;
            ogmios-run = self'.packages.ogmios-run;
            kupo = self'.packages.kupo;
            gen-wallet = self'.packages.gen-wallet;
            spammer = self'.packages.spammer;
            relay-node = self'.packages.relay-node;
            spo-node = self'.packages.spo-node;
            prometheus-run = self'.packages.prometheus-run;
          };
          arion-compose = pkgs'.arion'.build { modules = [ ./congested-testnet/arion-compose.nix ]; pkgs = pkgs'; };
        in
        pkgs.writeShellApplication {
          name = "start";
          runtimeInputs = [ pkgs'.arion' ];
          text = ''
            #!/bin/sh
            arion --prebuilt-file ${arion-compose} down -v  
            # arion --prebuilt-file ${arion-compose} up -d --remove-orphans
            # arion --prebuilt-file ${arion-compose} logs -f ogmios 
            # arion --prebuilt-file ${arion-compose} logs -f  testnet-config 
          '';
          # arion --prebuilt-file ${arion-compose} down  -v 
          # arion --prebuilt-file ${arion-compose} down spammer 
          # arion --prebuilt-file ${arion-compose} up -d spammer 
          # arion --prebuilt-file ${arion-compose} logs -f spammer 
        };
    };
}

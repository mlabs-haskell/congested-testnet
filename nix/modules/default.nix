{ self, inputs, ... }:
{

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
    arion-with-prebuilt = self.packages.${final.system}.arion-with-prebuilt;
    cardano-cli-remote-container = self.packages.${final.system}.cardano-cli-remote-container;
    update-kes = self.packages.${final.system}.update-kes;
    cardano-node = self.packages.${final.system}.cardano-node;
    share-config = self.packages.${final.system}.share-config;
    copy-config = self.packages.${final.system}.copy-config;

    add-ping = self.packages.${final.system}.add-ping;
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
    congested-testnet-dev = inputs.nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        inputs.arion.nixosModules.arion
        ./congested-testnet-dev
        { nixpkgs.overlays = [ self.overlays.default ]; }
      ];
    };
  };

  perSystem = { system, pkgs, self', inputs', ... }:
    {
      _module.args.pkgs = import inputs.nixpkgs { inherit system; overlays = [ self.overlays.default ]; };
      packages.vm = (self.nixosConfigurations.congested-testnet.extendModules {
        modules = [ (import ./congested-testnet/vm.nix) ];
      }).config.system.build.vm;

      packages.arion-compose = pkgs.arion.build { modules = [ ./congested-testnet/arion-compose.nix ]; inherit pkgs; };

      packages.arion-with-prebuilt = pkgs.writeShellApplication {
        name = "arion-with-prebuilt";
        runtimeInputs = [ pkgs.arion ];
        text = ''
          arion --prebuilt-file ${self'.packages.arion-compose} "$@" 
        '';
      };

      packages.add-ping =
        pkgs.writeShellApplication {
          name = "add-ping";
          text = ''docker exec testnet_node-spo-1_1 ${pkgs.iproute2}/bin/tc qdisc add dev eth0 root netem delay 500ms
          '';
        };
    };
}

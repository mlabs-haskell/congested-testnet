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
    ping-relay-spo = self.packages.${final.system}.ping-relay-spo;
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
      _module.args.pkgs = import inputs.nixpkgs {inherit system; overlays = [self.overlays.default];};
      packages.vm = (self.nixosConfigurations.congested-testnet.extendModules {
        modules = [ (import ./congested-testnet/vm.nix) ];
      }).config.system.build.vm;
      packages.arion = pkgs.arion;
      packages.run-all=
        let
          arion-compose = pkgs.arion.build { modules = [ ./congested-testnet/arion-compose.nix ]; inherit pkgs; };
        in
        pkgs.writeShellApplication {
          name = "run-all";
          runtimeInputs = [ pkgs.arion ];
          text = ''
            #!/bin/sh
            arion --prebuilt-file ${arion-compose} down -v --remove-orphans 
            arion --prebuilt-file ${arion-compose} up -d --remove-orphans
            arion --prebuilt-file ${arion-compose} logs -f node-relay-1 
          '';
        };
        # docker exec testnet_node-relay-1_1 /nix/store/qxr0j44p6cy69a00hympa2lakll14z97-iproute2-6.4.0/bin/tc qdisc add dev eth0 root netem delay 500ms
      packages.exec=
        let
          arion-compose = pkgs.arion.build { modules = [ ./congested-testnet/arion-compose.nix ]; inherit pkgs; };
        in
        pkgs.writeShellApplication {
          name = "exec";
          runtimeInputs = [ pkgs.arion ];
          text = ''
            #!/bin/sh
            arion --prebuilt-file ${arion-compose} exec testnet_node-relay-1_1 tc qdisc add dev eth0 root netem delay 100ms
          '';
        };
    };
}

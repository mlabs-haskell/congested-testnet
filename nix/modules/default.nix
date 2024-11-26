{ self, inputs, ... }:
{

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

{ inputs, self, ... }:
{
  imports = [
    ./overlays.nix
    ./cardano-node.nix
    ./spammer.nix
    ./docker.nix
    ./generate-scripts.nix
    ./ogmios-kupo.nix
    ./research.nix
    ./shell.nix
    ./tests.nix
  ];

  flake.nixosConfigurations = {
    congested-testnet = inputs.nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        # inputs.arion.nixosModules.arion
        ./nixos-module.nix
        { nixpkgs.overlays = [ self.overlays.default ]; }
      ];
    };
  };

  perSystem = { system, pkgs, self', inputs', ... }:
    {
      packages.vm = (self.nixosConfigurations.congested-testnet.extendModules {
        modules = [ (import ./vm.nix) ];
      }).config.system.build.vm;
    };
}

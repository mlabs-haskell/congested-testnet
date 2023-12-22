{ inputs, ... }:
{

  flake.nixosModules = {
    imports = [ ./desktop.nix ];
  };

  perSystem = { system, pkgs, self', ... }:
    {
      packages.vm = inputs.nixos-generators.nixosGenerate {
        inherit system;
        modules = [
          inputs.arion.nixosModules.arion
          ./vars.nix
          ./desktop.nix
          ./user.nix
          ./congested-testnet
        ];
        format = "vm";
      };
    };
}
# ${pkgs.arion}/bin/arion --prebuilt-file ${arion-compose} down  

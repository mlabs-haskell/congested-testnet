{ inputs,  ... }:
{
  flake.nixosModules = {
    imports = [ ./desktop.nix ];
  };

  perSystem = { system, pkgs, self',  ... }:
    {
      packages.vm = inputs.nixos-generators.nixosGenerate {
        inherit system;
        modules = [
          inputs.arion.nixosModules.arion
          ./vars.nix
          ./desktop.nix
          ./user.nix
          (import ./congested-testnet {inherit self';})
        ];
        format = "vm";
      };
      packages.check =
      let
       compose = pkgs.writeTextFile {
        name = "arion-compose";
        text = builtins.toJSON (import ./congested-testnet/arion-compose.nix {inherit pkgs self';});
       }; 
       pkgs' = pkgs // {faucet = self'.packages.faucet;};
      in
      # pkgs.arion.build { modules = [./congested-testnet/arion-compose.nix ]; inherit pkgs; };
      pkgs.arion.build { modules = [./congested-testnet/arion-compose.nix ]; pkgs =  pkgs'; };
    };
}

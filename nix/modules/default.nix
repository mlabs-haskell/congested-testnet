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
      packages.runnet =  ;
      packages.check =
        let
          pkgs' = pkgs // {
            arion' = self'.packages.arion;
            congested.faucet = self'.packages.faucet;
            congested.cardano-node = inputs'.cardano-node.legacyPackages.cardano-node;
            congested.testnet-conf = "/tmp/testnet-conf";
          };
          arion-compose = pkgs'.arion'.build { modules = [ ./congested-testnet/arion-compose.nix ]; pkgs = pkgs'; };
        in
        pkgs.writeShellApplication {
          name = "start";
          runtimeInputs = [ pkgs'.arion' pkgs.yq];
          text = ''
            #!/bin/sh
            mkdir -p /tmp/testnet-conf
            cp -r ${../../testnet-conf}/* /tmp/testnet-conf
            chmod o-rx /tmp/testnet-conf/pools/vrf1.skey 
            chmod g-rwx /tmp/testnet-conf/pools/vrf1.skey 
            jq < ${arion-compose}
            cp ${arion-compose} /tmp/debug.yaml
            
            arion --prebuilt-file ${arion-compose} down -v  
            arion --prebuilt-file ${arion-compose} up -d --remove-orphans
            arion --prebuilt-file ${arion-compose} logs -f 

          '';
            # docker-compose -f /tmp/test.yaml down -v  
            # docker-compose -f /tmp/test.yaml up -d --remove-orphans
            # docker-compose -f /tmp/test.yaml logs -f
            # jq < ${arion-compose} 
            # docker-compose -f ${arion-compose} down -v  
            # docker-compose -f ${arion-compose} up -d --remove-orphans
            # docker-compose -f ${arion-compose} logs -f 
        };
    };
}

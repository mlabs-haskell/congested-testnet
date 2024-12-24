{ inputs, self, ... }:
{
  perSystem = { system, self', pkgs, ... } : {
    # packages.congested-testnet-cli = pkgs.dockerTools.buildImage {
    # name = "congested-testnet-cli";
    # config.Cmd = "${self'.packages.gen-testnet-conf}/bin/gen-testnet-conf"; 
    # };
    packages.congested-testnet-cli = pkgs.writeShellApplication {
        name = "congested-testnet-cli";
        runtimeInputs = with pkgs; [
          fileshare
          coreutils
          cardano-node
          cardano-cli
          gen-testnet-conf
          openssl
        ];
        text = ''
         # random name folder
          ROOT="congested-testnet-$(openssl rand -hex 4)"
          export ROOT
          mkdir -p "$ROOT"
          echo "testnet configs in $ROOT"
          gen-testnet-conf "$ROOT"
          cardano-node run --config "$ROOT/configuration.yaml" \
              --database-path "$ROOT/db" \
              --port 3000 \
              --shelley-kes-key "$ROOT/pools-keys/pool1/kes.skey" \
              --shelley-operational-certificate "$ROOT/pools-keys/pool1/opcert.cert" \
              --shelley-vrf-key "$ROOT/pools-keys/pool1/vrf.skey" \
              --byron-signing-key  "$ROOT/byron-gen-command/delegate-keys.000.key" \
              --byron-delegation-certificate  "$ROOT/byron-gen-command/delegation-cert.000.json" \
              --host-addr "0.0.0.0" \
              --socket-path "$ROOT/node.socket" \
              --topology ${../containers/config/topology-spo-1.json}  
        '';
      };
  };
}

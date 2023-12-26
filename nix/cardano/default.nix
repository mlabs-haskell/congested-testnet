{ inputs, self, ... }:
{
  imports = [./aiken.nix];
  perSystem = { system, inputs', pkgs, ... }:
    {
      
      packages.change-testnet-conf =
        pkgs.writeShellApplication {
          name = "change-testnet-conf";
          runtimeInputs = [
            inputs'.cardano-node.legacyPackages.cardano-cli
            inputs'.cardano-node.legacyPackages.cardano-node
            pkgs.git
            pkgs.yq
          ];
          text =
            ''
               #!/bin/sh
               DIR=$(git rev-parse --show-toplevel)
               ROOT=$DIR/testnet-conf
               NETWORK_MAGIC=2
               SECURITY_PARAM=10
               NUM_SPO_NODES=1
               INIT_SUPPLY=10000000000
               START_TIME="$(date -d "now + 5 seconds" +%s)" 


               rm -rf "$ROOT/byron-gen-command"

               cardano-cli byron genesis genesis \
               --protocol-magic $NETWORK_MAGIC \
               --start-time "$START_TIME" \
               --k $SECURITY_PARAM \
               --n-poor-addresses 0 \
               --n-delegate-addresses $NUM_SPO_NODES \
               --total-balance $INIT_SUPPLY \
               --delegate-share 1 \
               --avvm-entry-count 0 \
               --avvm-entry-balance 0 \
               --protocol-parameters-file "$ROOT/byron.genesis.spec.json" \
               --genesis-output-dir "$ROOT/byron-gen-command"



               SUPPLY=16446744073709551615
               #max word64 integer
               MAX_SUPPLY=18446744073709551615


               cardano-cli genesis create-staked --genesis-dir "$ROOT" \
                 --testnet-magic "$NETWORK_MAGIC" \
                 --gen-pools 1 \
                 --supply $SUPPLY \
                 --supply-delegated $SUPPLY \
                 --gen-stake-delegs 1 \
                 --gen-utxo-keys 1 \
                 --gen-genesis-keys 1


              # change some parameters in shelley genesis

              jq --argjson maxSupply "$MAX_SUPPLY" --argjson secParam "$SECURITY_PARAM" '.maxLovelaceSupply = $maxSupply | .slotLength = 0.1 | .securityParam = $secParam | .activeSlotsCoeff = 0.1 | .securityParam = $secParam | .epochLength = 60 | .updateQuorum =2 | .protocolParams.protocolVersion.major = 7 | .protocolParams.minFeeA = 44 | .protocolParams.minFeeB = 155381 | .protocolParams.minUTxOValue = 1000000 | .protocolParams.decentralisationParam = 0.7 | .protocolParams.rho = 0.1 | .protocolParams.tau = 0.1' "$ROOT/genesis.json" > "/tmp/temp.json" && mv "/tmp/temp.json" "$ROOT/genesis.json"
            '';
        };
    };
}

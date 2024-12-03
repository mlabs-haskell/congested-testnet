{ inputs, self, ... }:
{
  perSystem = { system, inputs', pkgs, self', ... }:
    {
      packages.share-config = pkgs.writeShellApplication {
        name = "share-config";
        runtimeInputs = [
          pkgs.fileshare
          pkgs.coreutils
        ];
        text = ''
           ROOT=$1
           DIR=$2
           PORT=$3
           mkdir -p "$DIR/byron-gen-command"
           cp "$ROOT/byron-gen-command/genesis.json" "$DIR/byron-gen-command/"
           cp "$ROOT/genesis.json" "$DIR/"
           cp "$ROOT/genesis.alonzo.json" "$DIR/"
           cp "$ROOT/genesis.conway.json" "$DIR/"
           cp "$ROOT/configuration.yaml" "$DIR/"
           cp ${config/topology-relay-dev.json} "$DIR/topology-relay-dev.json"
           fileshare -p "$PORT" "$DIR"
        '';
      };

      packages.copy-config = pkgs.writeShellApplication {
        name = "copy-config";
        runtimeInputs = [
          pkgs.fileshare
          pkgs.coreutils
          pkgs.wget
        ];
        text = ''
           URL=$1
           OUT=$2
           cd "$OUT"
           wget -r -nH "$URL"
           touch "$OUT/finish_config" 
        '';
      };
      packages.gen-testnet-conf = pkgs.writeShellApplication {
        name = "gen-testnet-conf";
        runtimeInputs = [
          pkgs.jq
          pkgs.coreutils
          pkgs.cardano-node
          pkgs.cardano-cli
        ];
        text = ''
          ROOT=$1
          # chmod 777 -R "$ROOT"

          if [ -f "$ROOT/finish_config" ]; then
            exit 1
          fi

          cp ${./config/configuration.yaml} "$ROOT"/configuration.yaml
          cp ${./config/byron.genesis.spec.json} "$ROOT"/byron.genesis.spec.json
          cp ${./config/topology-relay-1.json} "$ROOT"/topology-relay-1.json
          cp ${./config/topology-relay-dev.json} "$ROOT"/topology-relay-dev.json
          cp ${./config/topology-spo-1.json} "$ROOT"/topology-spo-1.json

          NETWORK_MAGIC=42
          SECURITY_PARAM=2160
          NUM_SPO_NODES=1
          INIT_SUPPLY=10000000000
          START_TIME="$(date -d "now + 2 seconds" +%s)" 

          cardano-cli byron genesis genesis --protocol-magic "$NETWORK_MAGIC" \
           --start-time "$START_TIME" \
           --k "$SECURITY_PARAM" \
           --n-poor-addresses 0 \
           --n-delegate-addresses "$NUM_SPO_NODES" \
           --total-balance "$INIT_SUPPLY" \
           --delegate-share 1 \
           --avvm-entry-count 0 \
           --avvm-entry-balance 0 \
           --protocol-parameters-file "$ROOT/byron.genesis.spec.json" \
           --genesis-output-dir "$ROOT/byron-gen-command"



           MAX_SUPPLY=18346744073709551615


          cardano-cli conway genesis create-testnet-data \
                  --genesis-keys 1 \
                  --pools 1\
                  --stake-delegators 1\
                  --utxo-keys 1 \
                  --testnet-magic 42 \
                  --total-supply "$MAX_SUPPLY" \
                  --out-dir "$ROOT" 

          # change some parameters in shelley genesis

          jq --argjson maxSupply "$MAX_SUPPLY" --argjson secParam "$SECURITY_PARAM" '.maxLovelaceSupply = $maxSupply | .slotLength = 100 | .securityParam = $secParam | .activeSlotsCoeff = 0.05 | .epochLength = 432000 | .updateQuorum =2 | .protocolParams.protocolVersion.major = 9 | .protocolParams.minFeeA = 44 | .protocolParams.minFeeB = 155381 | .protocolParams.minUTxOValue = 1000000 | .protocolParams.decentralisationParam = 1 | .protocolParams.rho = 0.003 | .protocolParams.tau = 0.2 | .protocolParams.a0 = 0.3 | .protocolParams.maxBlockBodySize = 5000 | .protocolParams.maxBlockHeaderSize = 400' "$ROOT/shelley-genesis.json" > "$ROOT/temp.json" && mv "$ROOT/temp.json" "$ROOT/shelley-genesis.json"
          touch "$ROOT/finish_config" 
        '';
      };
};
}


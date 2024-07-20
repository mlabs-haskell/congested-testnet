{ inputs, self, ... }:
{
  perSystem = { system, inputs', pkgs, self', ... }:
    {
      packages.gen-testnet-conf = pkgs.writeShellApplication {
        name = "gen-testnet-conf";
        runtimeInputs = [
          pkgs.jq
          pkgs.coreutils
          self'.packages.cardano-node
        ];
        text = ''
          ROOT=$1
          if [ -f "$ROOT/finish_config" ]; then
            exit 1
          fi

          cp ${./config/configuration.yaml} "$ROOT"/configuration.yaml
          cp ${./config/genesis.alonzo.spec.json} "$ROOT"/genesis.alonzo.spec.json
          cp ${./config/genesis.conway.spec.json} "$ROOT"/genesis.conway.spec.json
          cp ${./config/byron.genesis.spec.json} "$ROOT"/byron.genesis.spec.json
          cp ${./config/genesis.alonzo.json} "$ROOT"/genesis.alonzo.json
          cp ${./config/genesis.conway.json} "$ROOT"/genesis.conway.json
          cp ${./config/genesis.spec.json} "$ROOT"/genesis.spec.json
          cp ${./config/topology-relay-1.json} "$ROOT"/topology-relay-1.json
          cp ${./config/topology-relay-dev.json} "$ROOT"/topology-relay-dev.json
          cp ${./config/topology-spo-1.json} "$ROOT"/topology-spo-1.json

          NETWORK_MAGIC=2
          SECURITY_PARAM=2160
          NUM_SPO_NODES=1
          INIT_SUPPLY=10000000000
          START_TIME="$(date -d "now + 1 seconds" +%s)" 

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
           MAX_SUPPLY=18346744073709551615

          chmod 777 -R "$ROOT"

           cardano-cli genesis create-staked --genesis-dir "$ROOT" \
             --testnet-magic "$NETWORK_MAGIC" \
             --gen-pools 1 \
             --supply $SUPPLY \
             --supply-delegated $SUPPLY \
             --gen-stake-delegs 1 \
             --gen-utxo-keys 1 \
             --gen-genesis-keys 1


          # change some parameters in shelley genesis

          jq --argjson maxSupply "$MAX_SUPPLY" --argjson secParam "$SECURITY_PARAM" '.maxLovelaceSupply = $maxSupply | .slotLength = 1 | .securityParam = $secParam | .activeSlotsCoeff = 0.05 | .securityParam = $secParam | .epochLength = 432000 | .updateQuorum =2 | .protocolParams.protocolVersion.major = 7 | .protocolParams.minFeeA = 44 | .protocolParams.minFeeB = 155381 | .protocolParams.minUTxOValue = 1000000 | .protocolParams.decentralisationParam = 0.7 | .protocolParams.rho = 0.003 | .protocolParams.tau = 0.2' "$ROOT/genesis.json" > "$ROOT/temp.json" && mv "$ROOT/temp.json" "$ROOT/genesis.json"

          touch "$ROOT/finish_config" 
        '';
      };

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
           mkdir "$DIR/byron-gen-command"
           cp "$ROOT/byron-gen-command/genesis.json" "$DIR/byron-gen-command/genesis.json"
           cp "$ROOT/genesis.json" "$DIR/genesis.json"
           cp "$ROOT/genesis.alonzo.json" "$DIR/genesis.alonzo.json"
           cp "$ROOT/genesis.conway.json" "$DIR/genesis.conway.json"
           cp "$ROOT/configuration.yaml" "$DIR/configuration.yaml"
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
        '';
      };
};
}


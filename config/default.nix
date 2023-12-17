inputs@{ pkgs, cardano, cardano-node, ... }:
{
  config = pkgs.writeShellApplication {
    name = "conf";
    runtimeInputs = [ cardano pkgs.git pkgs.yq ];
    text = ''
        #!/bin/sh
        # base script is located in 
        # https://github.com/input-output-hk/cardano-node/blob/{cardano-tag}/scripts/babbage/mkfiles.sh
        DIR=$(git rev-parse --show-toplevel)
        ROOT=$DIR/cardano-conf
        sudo rm -rf "$ROOT"
        mkdir -p "$ROOT"
        NETWORK_MAGIC=2
        SECURITY_PARAM=10
        NUM_SPO_NODES=1
        INIT_SUPPLY=10020000000
        START_TIME="$(date -d "now + 5 seconds" +%s)" 
        cat > "$ROOT/byron.genesis.spec.json" <<EOF
        {
          "heavyDelThd":     "300000000000",
          "maxBlockSize":    "2000000",
          "maxTxSize":       "4096",
          "maxHeaderSize":   "2000000",
          "maxProposalSize": "700",
          "mpcThd": "20000000000000",
          "scriptVersion": 0,
          "slotDuration": "1000",
          "softforkRule": {
            "initThd": "900000000000000",
            "minThd": "600000000000000",
            "thdDecrement": "50000000000000"
          },
          "txFeePolicy": {
            "multiplier": "43946000000",
            "summand": "155381000000000"
          },
          "unlockStakeEpoch": "18446744073709551615",
          "updateImplicit": "10000",
          "updateProposalThd": "100000000000000",
          "updateVoteThd": "1000000000000"
        }
      EOF
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

      # Because in Babbage the overlay schedule and decentralization parameter
      # are deprecated, we must use the "create-staked" cli command to create
      # SPOs in the ShelleyGenesis
      cp ${cardano-node}/scripts/babbage/alonzo-babbage-test-genesis.json "$ROOT/genesis.alonzo.spec.json"
      cp ${cardano-node}/scripts/babbage/conway-babbage-test-genesis.json "$ROOT/genesis.conway.spec.json"

      cp ${cardano-node}/configuration/defaults/byron-mainnet/configuration.yaml "$ROOT/"

      # shellcheck disable=SC2129
      # shellcheck disable=SC2215
      sed -i "$ROOT/configuration.yaml" \
           -e 's/Protocol: RealPBFT/Protocol: Cardano/' \
           -e '/Protocol/ aPBftSignatureThreshold: 0.6' \
           -e 's/minSeverity: Info/minSeverity: Debug/' \
           -e 's|GenesisFile: genesis.json|ByronGenesisFile: genesis/byron/genesis.json|' \
           -e '/ByronGenesisFile/ aShelleyGenesisFile: genesis/shelley/genesis.json' \
           -e '/ByronGenesisFile/ aAlonzoGenesisFile: genesis/shelley/genesis.alonzo.json' \
           -e '/ByronGenesisFile/ aConwayGenesisFile: genesis/shelley/genesis.conway.json' \
           -e 's/RequiresNoMagic/RequiresMagic/' \
           -e 's/LastKnownBlockVersion-Major: 0/LastKnownBlockVersion-Major: 7/' \
           -e 's/LastKnownBlockVersion-Minor: 2/LastKnownBlockVersion-Minor: 0/'

      # sed -i '/^SocketPath:/d' "$ROOT/configuration.yaml" 
  
      chmod 777 "$ROOT/configuration.yaml"
      # shellcheck disable=SC2129
      echo "TestShelleyHardForkAtEpoch: 0" >> "$ROOT/configuration.yaml"
      echo "TestAllegraHardForkAtEpoch: 0" >> "$ROOT/configuration.yaml"
      echo "TestMaryHardForkAtEpoch: 0" >> "$ROOT/configuration.yaml"
      echo "TestAlonzoHardForkAtEpoch: 0" >> "$ROOT/configuration.yaml"
      echo "TestBabbageHardForkAtEpoch: 0" >> "$ROOT/configuration.yaml"
      # echo "TestConwayHardForkAtEpoch: 0" >> "$ROOT/configuration.yaml"
      echo "ExperimentalProtocolsEnabled: True" >> "$ROOT/configuration.yaml"
      echo "EnableP2P: True" >> "$ROOT/configuration.yaml"

      # !!!!!!!!!!!!!!!!!!!!!!!! copy manual corrected config
      cp ${./configuration.yaml} "$ROOT/configuration.yaml"


      # !!!!!!!!!!!!!!!!!!!!!!!! change Shelley genesis.json

      # Copy the cost mode

      SUPPLY=1000000000000 

      cardano-cli genesis create-staked --genesis-dir "$ROOT" \
        --testnet-magic "$NETWORK_MAGIC" \
        --gen-pools 1 \
        --supply $SUPPLY \
        --supply-delegated $SUPPLY \
        --gen-stake-delegs 1 \
        --gen-utxo-keys 1 \
        --gen-genesis-keys 1

      SPO_NODES="node-spo1 node-spo2"

      # create the node directories
      for NODE in $SPO_NODES; do

        mkdir "$ROOT/$NODE"

      done
      # Here we move all of the keys etc generated by create-staked
      # for the nodes to use

      # Move all genesis related files
      mkdir -p "$ROOT/genesis/byron" mkdir -p "$ROOT/genesis/shelley"

      cp "$ROOT/byron-gen-command/genesis.json" "$ROOT/genesis/byron/genesis-wrong.json"
      cp "$ROOT/genesis.alonzo.json" "$ROOT/genesis/shelley/genesis.alonzo.json"
      cp "$ROOT/genesis.conway.json" "$ROOT/genesis/shelley/genesis.conway.json"
      cp "$ROOT/genesis.json" "$ROOT/genesis/shelley/genesis.json"

      jq --raw-output '.protocolConsts.protocolMagic = 2' "$ROOT/genesis/byron/genesis-wrong.json" > "$ROOT/genesis/byron/genesis.json"

      rm "$ROOT/genesis/byron/genesis-wrong.json"
      cp "$ROOT/genesis/shelley/genesis.json" "$ROOT/genesis/shelley/copy-genesis.json"

      jq -M '. + {slotLength:0.1, securityParam:10, activeSlotsCoeff:0.1, securityParam:10, epochLength:500, maxLovelaceSupply:10000000000000, updateQuorum:2}' "$ROOT/genesis/shelley/copy-genesis.json" > "$ROOT/genesis/shelley/copy2-genesis.json"
      jq --raw-output '.protocolParams.protocolVersion.major = 7 | .protocolParams.minFeeA = 44 | .protocolParams.minFeeB = 155381 | .protocolParams.minUTxOValue = 1000000 | .protocolParams.decentralisationParam = 0.7 | .protocolParams.rho = 0.1 | .protocolParams.tau = 0.1' "$ROOT/genesis/shelley/copy2-genesis.json" > "$ROOT/genesis/shelley/genesis.json"

      rm "$ROOT/genesis/shelley/copy2-genesis.json"
      rm "$ROOT/genesis/shelley/copy-genesis.json"


      # shellcheck disable=SC2086
      cp "${./topology-spo-1.json}" "$ROOT/topology-spo-1.json"
      cp "${./topology-relay-1.json}" "$ROOT/topology-relay-1.json" 
      cp "${./topology-spo-2.json}" "$ROOT/topology-spo-2.json" 
      cp "${./topology-relay-2.json}" "$ROOT/topology-relay-2.json"
      cp "${./topology-passive-3.json}" "$ROOT/topology-passive-3.json" 

  
      cp "$ROOT/pools/vrf1.skey" "$ROOT/node-spo1/vrf.skey"
      # cp "$ROOT/pools/vrf2.skey" "$ROOT/node-spo2/vrf.skey"
      cp "$ROOT/pools/opcert1.cert" "$ROOT/node-spo1/opcert.cert"
      # cp "$ROOT/pools/opcert2.cert" "$ROOT/node-spo2/opcert.cert"

      cp "$ROOT/pools/kes1.skey" "$ROOT/node-spo1/kes.skey"
      # cp "$ROOT/pools/kes2.skey" "$ROOT/node-spo2/kes.skey"

      # Byron related

      cp "$ROOT/byron-gen-command/delegate-keys.000.key" "$ROOT/node-spo1/byron-delegate.key"
      # cp "$ROOT/byron-gen-command/delegate-keys.001.key" "$ROOT/node-spo2/byron-delegate.key"

      cp "$ROOT/byron-gen-command/delegation-cert.000.json" "$ROOT/node-spo1/byron-delegation.cert"
      # cp "$ROOT/byron-gen-command/delegation-cert.001.json" "$ROOT/node-spo2/byron-delegation.cert"

      # Prometheus

      cp "${./prometheus.yml}" "$ROOT/prometheus.yml" 

      # Spammer db initial script
      mkdir -p "$ROOT/init-sql-script/"
      cp "${./init-db.sql}" "$ROOT/init-sql-script/init-db.sql"
      
      # insert utxo1 row 
      PKEY="$(jq ".cborHex" < "$ROOT/utxo-keys/utxo1.skey" | tail -c 66 | head -c -2 )"
      PUBKEY="$(jq ".cborHex" < "$ROOT/utxo-keys/utxo1.vkey" | tail -c 66 | head -c -2)"
      PUBKEYHASH="$(cardano-cli address key-hash --payment-verification-key-file "$ROOT/utxo-keys/utxo1.vkey")"
      chmod 777 "$ROOT/init-sql-script/init-db.sql" 
      echo "INSERT INTO wallets (pkey, pubkey, pubkeyhash, time) VALUES 
      (decode('$PKEY','hex'), decode('$PUBKEY','hex'), decode('$PUBKEYHASH','hex'), NOW());" >> "$ROOT/init-sql-script/init-db.sql"
    '';
  };
}

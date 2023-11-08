{ pkgs, iohk-nix, cardano, cardano-world, system, cardano-node, ... }:
{
  config = pkgs.writeScriptBin "config" ''
    #!/bin/sh
    cd $(git rev-parse --show-toplevel)
    ROOT=cardano-conf
    sudo rm -rf $ROOT
    mkdir $ROOT 

    GENESIS_DIR=$ROOT/genesis
    NUM_GENESIS_KEYS=2
    TESTNET_MAGIC=2
    SECURITY_PARAM=432
    TEMPLATE_DIR=${cardano-world}/docs/environments/private

    ${pkgs.jq}/bin/jq '.blockVersionData' $TEMPLATE_DIR/byron-genesis.json > $ROOT/byron.json
  

     ${cardano}/bin/cardano-cli genesis create-cardano \
          --genesis-dir "$GENESIS_DIR" \
          --gen-genesis-keys "$NUM_GENESIS_KEYS" \
          --gen-utxo-keys 2 \
          --supply 11234567890123456 \
          --testnet-magic "$TESTNET_MAGIC" \
          --byron-template "$ROOT/byron.json" \
          --shelley-template "$TEMPLATE_DIR/shelley-genesis.json" \
          --alonzo-template "$TEMPLATE_DIR/alonzo-genesis.json" \
          --node-config-template "$TEMPLATE_DIR/config.json" 


    #### copy alonzo and conway from repo , and fix issues in node config 
    cp $TEMPLATE_DIR/alonzo-genesis.json $GENESIS_DIR/
    cp $TEMPLATE_DIR/conway-genesis.json $GENESIS_DIR/

    ALONZO_HASH=$(echo $(${pkgs.jq}/bin/jq '.AlonzoGenesisHash' $TEMPLATE_DIR/config.json) | tr -d '"')
    ${pkgs.jq}/bin/jq --arg HASH $ALONZO_HASH '.AlonzoGenesisHash = $HASH | .RequiresNetworkMagic = "RequiresMagic" | .TestEnableDevelopmentNetworkProtocols = true | .TestBabbageHardForkAtEpoch = 0 | .TraceBlockFetchClient = true | .TraceBlockFetchDecisions = true | .TraceBlockFetchProtocol = true | .TraceBlockFetchProtocolSerialised = true | .TraceBlockFetchServer = true | .TraceChainSyncBlockServer = true | .TraceChainSyncClient = true | .TraceChainSyncHeaderServer = true | .TraceChainSyncProtocol = true | .TraceHandshake = true | .TraceLocalChainSyncProtocol = true | .TraceLocalHandshake = true | .TraceLocalTxSubmissionProtocol = true | .TraceLocalTxSubmissionServer = true | .TraceMux = true | .TraceTxInbound = true | .TraceTxOutbound = true | .TraceTxSubmissionProtocol = true | .ExperimentalProtocolsEnabled = true' \
      $GENESIS_DIR/node-config.json > temp.json && cp temp.json $GENESIS_DIR/node-config.json


    cat $GENESIS_DIR/node-config.json | jq

    cp "${./topology-spo-1.json}" $ROOT/topology-spo-1.json 
    cp "${./topology-relay-1.json}" $ROOT/topology-relay-1.json 
    cp "${./topology-spo-2.json}" $ROOT/topology-spo-2.json 
    cp "${./topology-relay-2.json}" $ROOT/topology-relay-2.json 
    cp "${./topology-passive-3.json}" $ROOT/topology-passive-3.json 
  '';
  config2 = pkgs.writeShellApplication {
    name = "conf";
    runtimeInputs = [ cardano pkgs.git ];
    text = ''
        #!/bin/sh
        # base script is located in 
        # https://github.com/input-output-hk/cardano-node/blob/1.35.6/scripts/babbage/mkfiles.sh

        DIR=$(git rev-parse --show-toplevel)
        ROOT=$DIR/cardano-conf
        sudo rm -rf "$ROOT"
        mkdir -p "$ROOT"
        NETWORK_MAGIC=2
        SECURITY_PARAM=10
        NUM_SPO_NODES=2
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
           -e 's/RequiresNoMagic/RequiresMagic/' \
           -e 's/LastKnownBlockVersion-Major: 0/LastKnownBlockVersion-Major: 6/' \
           -e 's/LastKnownBlockVersion-Minor: 2/LastKnownBlockVersion-Minor: 0/'

      # sed -i '/^SocketPath:/d' "$ROOT/configuration.yaml" 
  
      chmod 777 "$ROOT/configuration.yaml"
      # shellcheck disable=SC2129
      echo "TestShelleyHardForkAtEpoch: 0" >> "$ROOT/configuration.yaml"
      echo "TestAllegraHardForkAtEpoch: 0" >> "$ROOT/configuration.yaml"
      echo "TestMaryHardForkAtEpoch: 0" >> "$ROOT/configuration.yaml"
      echo "TestAlonzoHardForkAtEpoch: 0" >> "$ROOT/configuration.yaml"
      echo "TestBabbageHardForkAtEpoch: 0" >> "$ROOT/configuration.yaml"
      echo "TestEnableDevelopmentNetworkProtocols: True" >> "$ROOT/configuration.yaml"
      echo "EnableP2P: True" >> "$ROOT/configuration.yaml"

      # Copy the cost mode


      cardano-cli genesis create-staked --genesis-dir "$ROOT" \
        --testnet-magic "$NETWORK_MAGIC" \
        --gen-pools 2 \
        --supply 1000000000000 \
        --supply-delegated 1000000000000 \
        --gen-stake-delegs 2 \
        --gen-utxo-keys 2

      SPO_NODES="node-spo1 node-spo2"

      # create the node directories
      for NODE in $SPO_NODES; do

        mkdir "$ROOT/$NODE"

      done
      # Here we move all of the keys etc generated by create-staked
      # for the nodes to use

      # Move all genesis related files
      mkdir -p "$ROOT/genesis/byron"
      mkdir -p "$ROOT/genesis/shelley"

      cp "$ROOT/byron-gen-command/genesis.json" "$ROOT/genesis/byron/genesis-wrong.json"
      cp "$ROOT/genesis.alonzo.json" "$ROOT/genesis/shelley/genesis.alonzo.json"
      cp "$ROOT/genesis.json" "$ROOT/genesis/shelley/genesis.json"

      jq --raw-output '.protocolConsts.protocolMagic = 2' "$ROOT/genesis/byron/genesis-wrong.json" > "$ROOT/genesis/byron/genesis.json"

      rm "$ROOT/genesis/byron/genesis-wrong.json"

      sed -i "$ROOT/genesis/shelley/genesis.json" \
        -e 's/"slotLength": 1/"slotLength": 0.1/' \
        -e 's/"activeSlotsCoeff": 5.0e-2/"activeSlotsCoeff": 0.1/' \
        -e 's/"securityParam": 2160/"securityParam": 10/' \
        -e 's/"epochLength": 432000/"epochLength": 500/' \
        -e 's/"maxLovelaceSupply": 0/"maxLovelaceSupply": 1000000000000/' \
        -e 's/"minFeeA": 1/"minFeeA": 44/' \
        -e 's/"minFeeB": 0/"minFeeB": 155381/' \
        -e 's/"minUTxOValue": 0/"minUTxOValue": 1000000/' \
        -e 's/"decentralisationParam": 1.0/"decentralisationParam": 0.7/' \
        -e 's/"major": 0/"major": 7/' \
        -e 's/"rho": 0.0/"rho": 0.1/' \
        -e 's/"tau": 0.0/"tau": 0.1/' \
        -e 's/"updateQuorum": 5/"updateQuorum": 2/'

      # shellcheck disable=SC2086
      cp "${./topology-spo-1.json}" "$ROOT/topology-spo-1.json"
      cp "${./topology-relay-1.json}" "$ROOT/topology-relay-1.json" 
      cp "${./topology-spo-2.json}" "$ROOT/topology-spo-2.json" 
      cp "${./topology-relay-2.json}" "$ROOT/topology-relay-2.json"
      cp "${./topology-passive-3.json}" "$ROOT/topology-passive-3.json" 

  
      cp "$ROOT/pools/vrf1.skey" "$ROOT/node-spo1/vrf.skey"
      cp "$ROOT/pools/vrf2.skey" "$ROOT/node-spo2/vrf.skey"
      cp "$ROOT/pools/opcert1.cert" "$ROOT/node-spo1/opcert.cert"
      cp "$ROOT/pools/opcert2.cert" "$ROOT/node-spo2/opcert.cert"

      cp "$ROOT/pools/kes1.skey" "$ROOT/node-spo1/kes.skey"
      cp "$ROOT/pools/kes2.skey" "$ROOT/node-spo2/kes.skey"

      #Byron related

      cp "$ROOT/byron-gen-command/delegate-keys.000.key" "$ROOT/node-spo1/byron-delegate.key"
      cp "$ROOT/byron-gen-command/delegate-keys.001.key" "$ROOT/node-spo2/byron-delegate.key"
      # cp "$ROOT/byron-gen-command/delegate-keys.002.key" "$ROOT/node-spo3/byron-delegate.key"

      cp "$ROOT/byron-gen-command/delegation-cert.000.json" "$ROOT/node-spo1/byron-delegation.cert"
      cp "$ROOT/byron-gen-command/delegation-cert.001.json" "$ROOT/node-spo2/byron-delegation.cert"
      # cp "$ROOT/byron-gen-command/delegation-cert.002.json" "$ROOT/node-spo3/byron-delegation.cert"


      echo 3001 > "$ROOT/node-spo1/port"
      echo 3002 > "$ROOT/node-spo2/port"
      # echo 3003 > "$ROOT/node-spo3/port"

      # Make topology files
      # Make topology files
      #TODO generalise this over the N BFT nodes and pool nodes
      cat > "$ROOT/node-spo1/topology.json" <<EOF
      {
         "Producers": [
           {
             "addr": "127.0.0.1",
             "port": 3002,
             "valency": 1
           }
         , {
             "addr": "127.0.0.1",
             "port": 3003,
             "valency": 1
           }
         ]
       }
      EOF

      cat > "$ROOT/node-spo2/topology.json" <<EOF
      {
         "Producers": [
           {
             "addr": "127.0.0.1",
             "port": 3001,
             "valency": 1
           }
         , {
             "addr": "127.0.0.1",
             "port": 3003,
             "valency": 1
           }
         ]
       }
      EOF


      for NODE in $SPO_NODES; do
        (
          echo "#!/usr/bin/env bash"
          echo ""
          echo "cardano-node run \\"
          echo "  --config                          '$ROOT/configuration.yaml' \\"
          echo "  --topology                        '$ROOT/$NODE/topology.json' \\"
          echo "  --database-path                   '$ROOT/$NODE/db' \\"
          echo "  --socket-path                     '$ROOT/$NODE/node.sock' \\"
          echo "  --shelley-kes-key                 '$ROOT/$NODE/kes.skey' \\"
          echo "  --shelley-vrf-key                 '$ROOT/$NODE/vrf.skey' \\"
          echo "  --byron-delegation-certificate    '$ROOT/$NODE/byron-delegation.cert' \\"
          echo "  --byron-signing-key               '$ROOT/$NODE/byron-delegate.key' \\"
          echo "  --shelley-operational-certificate '$ROOT/$NODE/opcert.cert' \\"
          echo "  --port                            $(cat "$ROOT/$NODE/port") \\"
          echo "  | tee -a '$ROOT/$NODE/node.log'"
        ) > "$ROOT/$NODE.sh"

        chmod a+x "$ROOT/$NODE.sh"

        echo "$ROOT/$NODE.sh"
      done

      mkdir -p "$ROOT/run"

      echo "#!/usr/bin/env bash" > "$ROOT/run/all.sh"
      echo "" >> "$ROOT/run/all.sh"

      for NODE in $SPO_NODES; do
        echo "$ROOT/$NODE.sh &" >> "$ROOT/run/all.sh"
      done
      echo "" >> "$ROOT/run/all.sh"
      echo "wait" >> "$ROOT/run/all.sh"

      chmod a+x "$ROOT/run/all.sh"

      echo "CARDANO_NODE_SOCKET_PATH=$ROOT/node-spo1/node.sock "

      (cd "$ROOT"; ln -s node-spo1/node.sock main.sock)

    '';
  };
}

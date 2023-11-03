{ pkgs, iohk-nix, cardano, cardano-world, system, ... }:
pkgs.writeScriptBin "config" ''
  #!/bin/sh
  cd $(git rev-parse --show-toplevel)
  ROOT=cardano-conf
  sudo rm -rf $ROOT
  mkdir $ROOT 

  GENESIS_DIR=$ROOT/genesis
  NUM_GENESIS_KEYS=2
  TESTNET_MAGIC=2
  SECURITY_PARAM=432
  SLOT_LENGTH=1000 
  START_TIME="2023-10-01T14:00:00Z"
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
  ${pkgs.jq}/bin/jq --arg HASH $ALONZO_HASH '.AlonzoGenesisHash = $HASH | .RequiresNetworkMagic = "RequiresNoMagic" | .TestEnableDevelopmentNetworkProtocols = true' \
    $GENESIS_DIR/node-config.json > temp.json &&
                                                mv temp.json $GENESIS_DIR/node-config.json


  cat $GENESIS_DIR/node-config.json | jq

  cp "${./topology-spo-1.json}" $ROOT/topology-spo-1.json 
  cp "${./topology-relay-1.json}" $ROOT/topology-relay-1.json 
  cp "${./topology-spo-2.json}" $ROOT/topology-spo-2.json 
  cp "${./topology-relay-2.json}" $ROOT/topology-relay-2.json 
  cp "${./topology-passive-3.json}" $ROOT/topology-passive-3.json 
''

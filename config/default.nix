{ pkgs, iohk-nix, cardano, system, ... }:
pkgs.writeScriptBin "config" ''
  #!/bin/sh
  cd $(git rev-parse --show-toplevel)
  ROOT=cardano-conf
  mkdir $ROOT 
  GENESIS_DIR=$ROOT/genesis
  NUM_GENESIS_KEYS=2
  TESTNET_MAGIC=2
  SECURITY_PARAM=432
  SLOT_LENGTH=1000 
  START_TIME="2023-10-01T14:00:00Z"
  TEMPLATE_DIR=${iohk-nix}/cardano-lib/testnet-template

   ${cardano}/bin/cardano-cli genesis create-cardano \
        --genesis-dir "$GENESIS_DIR" \
        --gen-genesis-keys "$NUM_GENESIS_KEYS" \
        --gen-utxo-keys 2 \
        --testnet-magic "$TESTNET_MAGIC" \
        --supply 11234567890123456 \
        --byron-template "$TEMPLATE_DIR/byron.json" \
        --shelley-template "$TEMPLATE_DIR/shelley.json" \
        --alonzo-template "$TEMPLATE_DIR/alonzo.json" \
        --node-config-template "$TEMPLATE_DIR/config.json" \
        --security-param "$SECURITY_PARAM" \
        --conway-template "$TEMPLATE_DIR/conway.json" \

  cp "${./topology-spo-1.json}" $ROOT/topology-spo-1.json 
  cp "${./topology-relay-1.json}" $ROOT/topology-relay-1.json 
  cp "${./topology-spo-2.json}" $ROOT/topology-spo-2.json 
  cp "${./topology-relay-2.json}" $ROOT/topology-relay-2.json 
  cp "${./topology-passive-3.json}" $ROOT/topology-passive-3.json 
  ${pkgs.jq}/bin/jq '.RequiresNetworkMagic = "RequiresNoMagic"' $ROOT/genesis/node-config.json > temp.json &&
                                                mv temp.json $ROOT/genesis/node-config.json
''

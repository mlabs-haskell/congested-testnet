{pkgs,iohk-nix, ...}:
pkgs.writeScriptBin "config" ''
  #!/bin/sh
  ROOT=cardano-conf
  rm -rf $ROOT 
  mkdir $ROOT 
  GENESIS_DIR=$ROOT/genesis
  NUM_GENESIS_KEYS=2
  TESTNET_MAGIC=2
  SECURITY_PARAM=432
  SLOT_LENGTH=1000 
  START_TIME="2023-01-01T14:00:00Z"
  TEMPLATE_DIR=${iohk-nix}/cardano-lib/testnet-template

  cardano-cli genesis create-cardano \
        --genesis-dir "$GENESIS_DIR" \
        --gen-genesis-keys "$NUM_GENESIS_KEYS" \
        --gen-utxo-keys 3 \
        --supply 1234567890123456 \
        --testnet-magic "$TESTNET_MAGIC" \
        --slot-coefficient 0.05 \
        --byron-template "$TEMPLATE_DIR/byron.json" \
        --shelley-template "$TEMPLATE_DIR/shelley.json" \
        --alonzo-template "$TEMPLATE_DIR/alonzo.json" \
        --conway-template "$TEMPLATE_DIR/conway.json" \
        --node-config-template "$TEMPLATE_DIR/config.json" \
        --security-param "$SECURITY_PARAM" \
        --slot-length "$SLOT_LENGTH" \
        --start-time "$START_TIME"

  cp "${./topology-spo-1.json}" $ROOT/topology-spo-1.json 
  cp "${./topology-relay-1.json}" $ROOT/topology-relay-1.json 
  cp "${./topology-spo-2.json}" $ROOT/topology-spo-2.json 
  cp "${./topology-relay-2.json}" $ROOT/topology-relay-2.json 
  cp "${./topology-passive-3.json}" $ROOT/topology-passive-3.json 

  #genesis addresses
  cardano-cli address build \
    --payment-verification-key-file "$GENESIS_DIR/utxo-keys/shelley.000.vkey" \
    --out-file "$ROOT/payment.addr" \
    --testnet-magic $TESTNET_MAGIC

''

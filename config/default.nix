{pkgs,iohk-nix, ...}:
let 
topology =pkgs.writeText "topology" ''
{
  "localRoots": [
    {
      "accessPoints": [],
      "advertise": false,
      "valency": 1
    }
  ],
  "publicRoots": [
    {
      "accessPoints": [
        {
          "address": "preview-node.world.dev.cardano.org",
          "port": 30002
        }
      ],
      "advertise": false
    }
  ],
  "useLedgerAfterSlot": 322000
}
'';
in
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
        --gen-utxo-keys 1 \
        --supply 30000000000000000 \
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

  cp "${topology}" $ROOT/topology.json 

''

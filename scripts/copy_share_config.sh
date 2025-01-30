#!/bin/sh

# copy some config files to share directory
cp $CONFIGURATION_YAML $ROOT/configuration.yaml
mkdir -p "$SHARE/byron-gen-command"
cp "$ROOT/shelley-genesis.json" "$SHARE"
cp "$ROOT/byron-gen-command/genesis.json" "$SHARE/byron-gen-command/genesis.json"
cp "$ROOT/conway-genesis.json" "$SHARE"
cp "$ROOT/alonzo-genesis.json" "$SHARE"
cp "$ROOT/configuration.yaml" "$SHARE"

# extract protocol params
cardano-cli conway query protocol-parameters \ 
  --socket-path testnet_data/node.socket \
  --testnet-magic 42  > "$SHARE/protocol.json"


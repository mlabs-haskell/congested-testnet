#!/bin/sh
# export ROOT
kupo \
  --node-config $ROOT/configuration.yaml \
  --node-socket $ROOT/node.socket \
  --since origin \
  --match "*/*" \
  --host 0.0.0.0 \
  --workdir $ROOT \
  --prune-utxo \
  --defer-db-indexes 

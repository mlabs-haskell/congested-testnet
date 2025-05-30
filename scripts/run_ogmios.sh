#!/bin/sh
# export ROOT
ogmios \
  --host 0.0.0.0 \
  --port 1337 \
  --node-socket $ROOT/node.socket \
  --node-config $ROOT/configuration.yaml \
  --include-transaction-cbor

#!/bin/sh
# export TOPOLOGY_GENESIS_SPO_JSON

ROOT=$1
PORT=$2
TOPOLOGY_SPO_JSON=$3

cardano-node run --config "$ROOT/configuration.yaml" \
    --database-path "$ROOT/db" \
    --port $PORT \
    --shelley-kes-key "$ROOT/kes.skey" \
    --shelley-operational-certificate "$ROOT/opcert.cert" \
    --shelley-vrf-key "$ROOT/vrf.skey" \
    --host-addr "0.0.0.0" \
    --socket-path "$ROOT/node.socket" \
    --topology $TOPOLOGY_SPO_JSON

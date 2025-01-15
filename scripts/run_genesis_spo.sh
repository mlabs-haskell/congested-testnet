#!/bin/sh
# export TOPOLOGY_GENESIS_SPO_JSON
# export ROOT 
# export PORT 

cardano-node run --config "$ROOT/configuration.yaml" \
    --database-path "$ROOT/db" \
    --port $PORT \
    --shelley-kes-key "$ROOT/pools-keys/pool1/kes.skey" \
    --shelley-operational-certificate "$ROOT/pools-keys/pool1/opcert.cert" \
    --shelley-vrf-key "$ROOT/pools-keys/pool1/vrf.skey" \
    --byron-signing-key  "$ROOT/byron-gen-command/delegate-keys.000.key" \
    --byron-delegation-certificate  "$ROOT/byron-gen-command/delegation-cert.000.json" \
    --host-addr "0.0.0.0" \
    --socket-path "$ROOT/node.socket" \
    --topology $TOPOLOGY_GENESIS_SPO_JSON 

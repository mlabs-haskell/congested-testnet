#!/bin/sh
# run spo node without byron genesis
ROOT=$1
PORT=$2
ACCESS_URL=$3
cd $ROOT
cat <<EOF > topology.json
{
  "publicRoots": [
    {
      "accessPoints": [
        {
          "address": "${ACCESS_URL}",
          "port": 3000
        }
      ],
      "advertise": true 
    }
  ],
  "localRoots": [],
  "useLedgerAfterSlot":-1
}
EOF
cat topology.json

cardano-node run --config "configuration.yaml" \
    --database-path "db" \
    --port $PORT \
    --host-addr "0.0.0.0" \
    --socket-path "node.socket" \
    --topology "topology.json" 
    # --shelley-kes-key "$ROOT/kes.skey" \
    # --shelley-operational-certificate "$ROOT/opcert.cert" \
    # --shelley-vrf-key "$ROOT/vrf.skey" \

# cardano-node run --config "$ROOT/configuration.yaml" \
#     --database-path "$ROOT/db" \
#     --port $PORT \
#     --shelley-kes-key "$ROOT/kes.skey" \
#     --shelley-operational-certificate "$ROOT/opcert.cert" \
#     --shelley-vrf-key "$ROOT/vrf.skey" \
#     --host-addr "0.0.0.0" \
#     --socket-path "$ROOT/node.socket" \
#     --topology "topology.json" 

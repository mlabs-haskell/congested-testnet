#!/bin/sh
# testnet config folder
# export SRC
# staking node data/config folder
# export DIR
# staking node running port
# export PORT
# topology access url
# export ACCESS_URL

# copy files for staking node
# mkdir -p "$DIR"
# mkdir -p "$DIR/byron-gen-command"
# cp "$SRC/shelley-genesis.json" "$DIR"
# cp "$SRC/byron-gen-command/genesis.json" "$DIR/byron-gen-command/genesis.json"
# cp "$SRC/conway-genesis.json" "$DIR"
# cp "$SRC/alonzo-genesis.json" "$DIR"
# cp "$SRC/configuration.yaml" "$DIR"

cd $SRC
cat <<EOF > topology.json
{
  "publicRoots": [
    {
      "accessPoints": [
        {
          "address": ${ACCESS_URL},
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
cardano-node run --config "configuration.yaml" \
    --database-path "db1" \
    --port $PORT \
    --host-addr 0.0.0.0 \
    --topology "topology.json" 
    # --socket-path "node.socket" \
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

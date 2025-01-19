#!/bin/sh
# testnet config folder
# export SRC
# staking node data/config folder
# export DATA
# staking node running port
# export PORT
# topology access url
# export ACCESS_URL

# copy files for staking node
#
mkdir -p "$DATA/byron-gen-command"
cp "$CONFIG/shelley-genesis.json" "$DATA"
cp "$CONFIG/byron-gen-command/genesis.json" "$DATA/byron-gen-command/genesis.json"
cp "$CONFIG/conway-genesis.json" "$DATA"
cp "$CONFIG/alonzo-genesis.json" "$DATA"
cp "$CONFIG/configuration.yaml" "$DATA"

cd $DATA
cat <<EOF > topology.json
{
  "publicRoots": [
  {
  "accessPoints": [
   {
    "address":"genesis.local",
    "port":3000
   }
  ],
  "advertise": true 
  }
  ],
  "localRoots":[],
  "useLedgerAfterSlot": 1 
}
EOF
  # "useLedgerAfterSlot":-1

cardano-node run --config "configuration.yaml" \
    --database-path "db" \
    --port $PORT \
    --host-addr 0.0.0.0 \
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
    # {
    #   "accessPoints": [
    #     {
    #       "address":"genesis.local",
    #       "port":3000
    #     }
    #   ],
    #   "advertise":false,
    #   "valency":1
    # }
    # {
    #   "accessPoints": [
    #     {
    #       "address":"staking.local",
    #       "port":3000
    #     }
    #   ],
    #   "advertise":true, 
    #   "valency":1
    # }

#!/bin/sh

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

cardano-node run --config "configuration.yaml" \
    --database-path "db" \
    --port $PORT \
    --host-addr 0.0.0.0 \
    --socket-path "node.socket" \
    --topology "topology.json" 

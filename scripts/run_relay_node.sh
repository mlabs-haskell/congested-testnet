#!/bin/sh

cd $DATA
mkdir -p "byron-gen-command"
wget -O "shelley-genesis.json" "$SPO_ADDRESS:5000/shelley-genesis.json"
wget -O "byron-gen-command/genesis.json" "$SPO_ADDRESS:5000/byron-gen-command/genesis.json"
wget -O "conway-genesis.json" "$SPO_ADDRESS:5000/conway-genesis.json"
wget -O "alonzo-genesis.json" "$SPO_ADDRESS:5000/alonzo-genesis.json"
wget -O "configuration.yaml" "$SPO_ADDRESS:5000/configuration.yaml"

cat <<EOF > "topology.json"
{
  "publicRoots": [
  {
  "accessPoints": [
   {
    "address":"$SPO_ADDRESS",
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

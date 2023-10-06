#!/bin/sh
rm -rf tmp
mkdir tmp
ROOT=./tmp
GENESIS_DIR=../cardano-conf/genesis
SOCKETS=../cardano-conf/sockets
cd $ROOT

#generate user1 wallet 
cardano-cli address key-gen \
    --verification-key-file user1.vkey \
    --signing-key-file user1.skey

cardano-cli address build \
    --payment-verification-key-file user1.vkey \
    --out-file user1.addr \
    --testnet-magic 2

#extract genesis node spo 1 wallet address

#genesis address
cardano-cli address build \
  --payment-verification-key-file $GENESIS_DIR/utxo-keys/shelley.000.vkey \
  --out-file genesis-1.addr \
  --testnet-magic 2 


echo "send ada from genesis wallet to user wallet"

cardano-cli query protocol-parameters \
      --socket-path "$SOCKETS/node-relay-1-socket/node.socket" \
	    --testnet-magic 2 \
	    --out-file "protocol-parameters.json"

cardano-cli query utxo \
      --socket-path "$SOCKETS/node-relay-1-socket/node.socket" \
	    --testnet-magic 2 \
	    --address $(cat genesis-1.addr) \
	    --out-file genesis-utxos.json

cat genesis-utxos.json

TXIN=$(jq "keys[0]" "genesis-utxos.json" --raw-output)
SEND_AMT=500000000
TXOUT="$(cat user1.addr)+${SEND_AMT}"

cardano-cli transaction build \
      --socket-path "$SOCKETS/node-relay-1-socket/node.socket" \
      --alonzo-era \
	    --testnet-magic 2 \
	    --change-address $(cat genesis-1.addr) \
	    --tx-in "${TXIN}" \
	    --tx-out "${TXOUT}" \
	    --out-file "tx.body" \
      --witness-override 2

cat "tx.body"

cardano-cli transaction sign \
	    --tx-body-file "tx.body" \
	    --signing-key-file "${GENESIS_DIR}/utxo-keys/shelley.000.skey" \
	    --testnet-magic 2 \
	    --out-file "tx.signed"

cardano-cli transaction submit \
      --socket-path "$SOCKETS/node-relay-1-socket/node.socket" \
	    --tx-file "tx.signed" \
	    --testnet-magic 2
echo "transaction from genesis utxo to user-1 wallet is ok "

echo OK 

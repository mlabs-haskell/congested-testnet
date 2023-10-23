#!/bin/sh
alias cardano-cli=$CARDANO_CLI/bin/cardano-cli 
ROOT=./tmp
GENESIS_DIR=../cardano-conf/genesis
SOCKETS=../cardano-conf/sockets

if [ -d "$ROOT" ]; then
    echo "tmp exists."
else
    mkdir $ROOT
fi
cd $ROOT


echo "send ada from user wallet to user wallet"

cardano-cli query protocol-parameters \
      --socket-path "$SOCKETS/node-relay-1-socket/node.socket" \
	    --testnet-magic 2 \
	    --out-file "protocol-parameters.json"
cardano-cli query utxo \
      --socket-path "$SOCKETS/node-relay-1-socket/node.socket" \
	    --testnet-magic 2 \
	    --address $(cat wallet0.addr) \
	    --out-file user-utxos.json

cat user-utxos.json

TXIN=$(jq "keys[0]" "user-utxos.json" --raw-output)
SEND_AMT=6543210
TXOUT="$(cat wallet0.addr)+${SEND_AMT}"

cardano-cli transaction build \
      --socket-path "$SOCKETS/node-relay-1-socket/node.socket" \
      --alonzo-era \
	    --testnet-magic 2 \
	    --change-address $(cat wallet0.addr) \
	    --tx-in "${TXIN}" \
	    --tx-out "${TXOUT}" \
	    --out-file "tx.body" \
      --witness-override 2

cat "tx.body"

cardano-cli transaction sign \
	    --tx-body-file "tx.body" \
	    --signing-key-file "wallet0.skey" \
	    --testnet-magic 2 \
	    --out-file "tx.signed"

cat "tx.signed"

# cardano-cli transaction submit \
#       --socket-path "$SOCKETS/node-relay-1-socket/node.socket" \
# 	    --tx-file "tx.signed" \
# 	    --testnet-magic 2
# echo "transaction from genesis utxo to user-1 wallet is ok "
#
# echo OK 

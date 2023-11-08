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

# #generate wallet0 wallet 

cardano-cli address key-gen \
    --verification-key-file wallet0.vkey \
    --signing-key-file wallet0.skey

cardano-cli address build \
    --payment-verification-key-file wallet0.vkey \
    --out-file wallet0.addr \
    --testnet-magic 2

#extract genesis node spo 1 wallet address

genesis address
cardano-cli address build \
  --payment-verification-key-file $GENESIS_DIR/utxo-keys/shelley.000.vkey \
  --out-file genesis-1.addr \
  --testnet-magic 2 

# cardano-cli signing-key-address \
#     --testnet-magic 2 \
#     --secret "$GENESIS_DIR/utxo-keys/byron.000.key" > genesis-1.addr
#
# cardano-cli issue-genesis-utxo-expenditure \
#     --genesis-json $GENESIS_DIR/byron-genesis.json \
#     --testnet-magic 2 \
#     --tx "tx0.tx" \
#     --wallet-key "$GENESIS_DIR/utxo-keys/byron.000.key" \
#     --rich-addr-from $(head -n 1 "genesis-1.addr") \
#     --txout "(\"$(head -n 1 wallet0.addr)\", 5000000)"


echo "send ada from genesis wallet to user wallet"

export CARDANO_NODE_SOCKET_PATH=$SOCKETS/node-relay-1-socket/node.socket 
cardano-cli query protocol-parameters \
	    --testnet-magic 2 \
	    --out-file "protocol-parameters.json"

export CARDANO_NODE_SOCKET_PATH=$SOCKETS/node-relay-1-socket/node.socket 
cardano-cli query utxo \
	    --testnet-magic 2 \
	    --address $(cat genesis-1.addr) \
	    --out-file genesis-utxos.json

cat genesis-utxos.json

TXIN=$(jq "keys[0]" "genesis-utxos.json" --raw-output)
SEND_AMT=500000000
SEND_AMT2=200000000
SEND_AMT3=100000000
TXOUT="$(cat wallet0.addr)+${SEND_AMT}"
TXOUT2="$(cat wallet0.addr)+${SEND_AMT2}"
TXOUT3="$(cat wallet0.addr)+${SEND_AMT3}"

export CARDANO_NODE_SOCKET_PATH=$SOCKETS/node-relay-1-socket/node.socket 
cardano-cli transaction build \
	    --testnet-magic 2 \
	    --change-address $(cat genesis-1.addr) \
	    --tx-in "${TXIN}" \
	    --tx-out "${TXOUT}" \
	    --tx-out "${TXOUT2}" \
	    --tx-out "${TXOUT3}" \
	    --out-file "tx.body" \
      --witness-override 2


cardano-cli transaction sign \
	    --tx-body-file "tx.body" \
	    --signing-key-file "${GENESIS_DIR}/utxo-keys/shelley.000.skey" \
	    --testnet-magic 2 \
	    --out-file "tx.signed"
cat "tx.signed"

cardano-cli transaction submit \
	    --tx-file "tx.signed" \
	    --testnet-magic 2
# # echo "transaction from genesis utxo to user-1 wallet is ok "
#
# echo OK 

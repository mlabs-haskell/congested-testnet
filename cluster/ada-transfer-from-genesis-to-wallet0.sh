#!/bin/sh


CONFIG=../cardano-conf
export CARDANO_NODE_SOCKET_PATH=$CONFIG/sockets/node-relay-1-socket/node.socket 

alias cardano-cli=$CARDANO_CLI/bin/cardano-cli 
ROOT=./tmp
rm -rf ./tmp

if [ -d "$ROOT" ]; then
    echo "tmp exists."
else
    mkdir $ROOT
fi
cd $ROOT

# Loop until network is ready 

#generate wallet0 wallet 

cardano-cli address key-gen \
    --verification-key-file wallet0.vkey \
    --signing-key-file wallet0.skey


cardano-cli address build \
    --payment-verification-key-file wallet0.vkey \
    --out-file wallet0.addr \
    --testnet-magic 2


# genesis address
cardano-cli address build \
  --payment-verification-key-file $CONFIG/utxo-keys/utxo1.vkey \
  --out-file genesis1.addr \
  --testnet-magic 2 

# cat $CONFIG/utxo-keys/utxo1.vkey
# cat genesis1.addr



# echo "send ada from genesis wallet to user wallet"
cardano-cli query utxo \
	    --testnet-magic 2 \
	    --address $(cat genesis1.addr) \
	    --out-file genesis-utxos.json

# cat genesis-utxos.json | jq

TXIN=$(jq "keys[0]" "genesis-utxos.json" --raw-output)
SEND_AMT=500000000
SEND_AMT2=200000000
SEND_AMT3=100000000
TXOUT="$(cat wallet0.addr)+${SEND_AMT}"
TXOUT2="$(cat wallet0.addr)+${SEND_AMT2}"
TXOUT3="$(cat wallet0.addr)+${SEND_AMT3}"

cardano-cli transaction build \
	    --testnet-magic 2 \
	    --change-address $(cat genesis1.addr) \
	    --tx-in "${TXIN}" \
	    --tx-out "${TXOUT}" \
	    --tx-out "${TXOUT2}" \
	    --tx-out "${TXOUT3}" \
	    --out-file "tx.body" \
      --witness-override 2


cardano-cli transaction sign \
	    --tx-body-file "tx.body" \
	    --signing-key-file "$CONFIG/utxo-keys/utxo1.skey" \
	    --testnet-magic 2 \
	    --out-file "tx.signed"

cardano-cli transaction submit \
	    --tx-file "tx.signed" \
	    --testnet-magic 2


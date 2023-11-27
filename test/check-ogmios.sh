#!/bin/sh
OGMIOS_REQUEST='{"params":{},"method":"queryLedgerState/utxo","jsonrpc":"2.0","id":"queryLedgerState/protocolParameters-5pyr568mlp9m1h8a"}'
  # "id" : "queryLedgerState/protocolParameters-5pyr5tknolp8mtrbe" 
# echo "Sending the following request to Ogmios"
# echo "$OGMIOS_REQUEST" | jq
# echo "OGMIOS RESULT"
# echo "$OGMIOS_REQUEST" | tr -d "\n" 
# echo "$OGMIOS_REQUEST" | tr -d "\n" | websocat ws://127.0.0.1:1337 | jq ".result"
echo "$OGMIOS_REQUEST" | tr -d "\n" | websocat ws://127.0.0.1:1337  | jq


# alias cardano-cli=$CARDANO_CLI/bin/cardano-cli 
# ROOT=./tmp
# CONF=../cardano-conf
# # GENESIS_DIR=../cardano-conf/genesis
# # SOCKETS=../cardano-conf/sockets
# cd $ROOT
# #
# export CARDANO_NODE_SOCKET_PATH=$CONF/node-spo1/node.socket 
# cardano-cli query tip --testnet-magic 2
#
# cardano-cli query utxo \
# 	    --testnet-magic 2 \
# 	    --address $(cat wallet0.addr) \
# 	    --out-file user1-utxos.json

# cat user1-utxos.json
#
# cardano-cli address build \
#     --payment-script-file "../validator.uplc" \
#     --testnet-magic 2\
#     --out-file "script.addr"

# cardano-cli query protocol-parameters \
# 	    --testnet-magic 2 \
# 	    --out-file "protocol-parameters.json"
#
# TXIN=$(jq "keys[0]" "user1-utxos.json" --raw-output)
# TXOUT="$(cat "script.addr") 2123456"

# echo $TXOUT
# cardano-cli transaction build \
#       --alonzo-era \
# 	    --testnet-magic 2 \
# 	    --change-address $(cat wallet0.addr) \
# 	    --tx-in "${TXIN}" \
# 	    --tx-out "${TXOUT}" \
# 	    --out-file "tx.body" \
#       --protocol-params-file "protocol-parameters.json" 
#
# cardano-cli transaction sign \
# 	    --tx-body-file "tx.body" \
# 	    --signing-key-file "wallet0.skey" \
# 	    --testnet-magic 2 \
# 	    --out-file "tx.signed"
#
# cardano-cli transaction submit \
# 	    --tx-file "tx.body" \
# 	    --testnet-magic 2

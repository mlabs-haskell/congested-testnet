#!/bin/sh
alias cardano-cli=$CARDANO_CLI/bin/cardano-cli 
ROOT=./tmp
GENESIS_DIR=../cardano-conf/genesis
SOCKETS=../cardano-conf/sockets
cd $ROOT

export CARDANO_NODE_SOCKET_PATH=$SOCKETS/node-passive-3-socket/node.socket 
cardano-cli query utxo \
	    --testnet-magic 2 \
	    --address $(cat wallet0.addr) \
	    --out-file user1-utxos.json
cat user1-utxos.json

	    # --address $(cat user1.addr) \
      # --socket-path "$SOCKETS/node-relay-1-socket/node.socket" \

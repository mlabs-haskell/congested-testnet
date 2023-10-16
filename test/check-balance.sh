#!/bin/sh
ROOT=./tmp
GENESIS_DIR=../cardano-conf/genesis
SOCKETS=../cardano-conf/sockets
cd $ROOT

cardano-cli query utxo \
      --socket-path "$SOCKETS/node-passive-3-socket/node.socket" \
	    --testnet-magic 2 \
	    --address $(cat user1.addr) \
	    --out-file user1-utxos.json
cat user1-utxos.json

	    # --address $(cat user1.addr) \
      # --socket-path "$SOCKETS/node-relay-1-socket/node.socket" \

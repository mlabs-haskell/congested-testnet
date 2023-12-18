#!/bin/sh
alias cardano-cli=$CARDANO_CLI/bin/cardano-cli 
ROOT=./tmp

CONFIG=../cardano-conf/
GENESIS_DIR=../cardano-conf/genesis
SOCKETS=../cardano-conf/sockets
cd $ROOT

export CARDANO_NODE_SOCKET_PATH=$SOCKETS/node-relay-1-socket/node.socket 
cardano-cli query utxo \
	    --testnet-magic 2 \
	    --address $(cat wallet0.addr) \
	    --out-file user1-utxos.json
cat user1-utxos.json

cardano-cli stake-address build \
  --stake-verification-key-file $CONFIG/pools/staking-reward1.vkey \
  --out-file stake.addr \
  --testnet-magic 2 

cardano-cli query stake-address-info \
  --address $(cat stake.addr) \
  --testnet-magic 2 


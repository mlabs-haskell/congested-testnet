#!/bin/sh
# defined outside vars:
SRC=$1
DIR=$2
mkdir -p "$DIR"
mkdir -p "$DIR/byron-gen-command"
cp "$SRC/shelley-genesis.json" "$DIR"
cp "$SRC/byron-gen-command/genesis.json" "$DIR/byron-gen-command"
cp "$SRC/conway-genesis.json" "$DIR"
cp "$SRC/alonzo-genesis.json" "$DIR"
cp "$SRC/configuration.yaml" "$DIR"
# cd $DIR
# wget -r -nH "$URL" wget -r --no-parent -nH --cut-dirs=1 -P . "$ROOT" wget -r -np -nH --cut-dirs=1 -P . "$ROOT/byron-genesis-file/" cardano-cli conway address key-gen \
#   --verification-key-file payment.vkey \
#   --signing-key-file payment.skey
#
# cardano-cli conway stake-address key-gen \
#   --verification-key-file stake.vkey \
#   --signing-key-file stake.skey
#
# cardano-cli conway stake-address build \
#   --stake-verification-key-file stake.vkey \
#   --out-file stake.addr \
#   --testnet-magic 42
#
# cardano-cli conway node key-gen \
#   --cold-verification-key-file cold.vkey \
#   --cold-signing-key-file cold.skey \
#   --operational-certificate-issue-counter cold.counter
#
# cardano-cli conway node key-gen-KES \
#   --verification-key-file kes.vkey \
#   --signing-key-file kes.skey
#
# cardano-cli conway node key-gen-VRF \
#   --verification-key-file vrf.vkey \
#   --signing-key-file vrf.skey

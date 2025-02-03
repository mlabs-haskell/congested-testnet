#!/bin/sh

# 1. config config files to SHARE 
# 2. add aditional utxo for ctl spammer (simple pay transaction with cardano-cli) 


# 1 ++++++++++++++++++++++++++++++++++++++++++++++++++++++

# extract protocol params
#

while true; do
    cardano-cli conway query protocol-parameters \
        --socket-path "$ROOT/node.socket" \
        --testnet-magic 42 > "$SHARE/protocol.json"
    if [ $? -eq 0 ]; then
        echo "share testnet parameters"
        break
    else
        echo "Command failed. Retrying in 5 seconds..."
        sleep 2
    fi
done

cp $CONFIGURATION_YAML $ROOT/configuration.yaml
mkdir -p "$SHARE/byron-gen-command"
cp "$ROOT/shelley-genesis.json" "$SHARE"
cp "$ROOT/byron-gen-command/genesis.json" "$SHARE/byron-gen-command/genesis.json"
cp "$ROOT/conway-genesis.json" "$SHARE"
cp "$ROOT/alonzo-genesis.json" "$SHARE"
cp "$ROOT/configuration.yaml" "$SHARE"




# 2 ++++++++++++++++++++++++++++++++++++++++++++++++++++++
PKEYHEX=$(jq '.cborHex' < "$ROOT/utxo-keys/utxo1/utxo.skey")

echo "{\"type\":\"PaymentSigningKeyShelley_ed25519\",\"description\":\"Payment_Signing_Key\",\"cborHex\":$PKEYHEX}" > "$ROOT/wallet.skey"

echo "build address"

cardano-cli conway address build \
  --payment-verification-key-file "$ROOT/utxo-keys/utxo1/utxo.vkey" \
  --out-file "$ROOT/wallet.addr" \
  --testnet-magic 42 

if [ ! -f "$ROOT/wallet.addr" ]; then
  echo "Error: wallet address file was not created!" 
  exit 1
fi



cardano-cli conway query utxo \
      --socket-path "$ROOT/node.socket" \
      --testnet-magic 42 \
      --address "$(cat "$ROOT/wallet.addr")" \
      --out-file "$ROOT/utxos.json"

if [ ! -f "$ROOT/utxos.json" ]; then
  exit 1
fi

echo "query utxo"


TXIN=$(jq "keys[0]" "$ROOT/utxos.json" --raw-output)
SEND_AMT=3000000
TXOUT="$(cat "$ROOT/wallet.addr")+$SEND_AMT"

cardano-cli conway transaction build \
      --socket-path "$ROOT/node.socket" \
      --testnet-magic 42 \
      --change-address "$(cat "$ROOT/wallet.addr")" \
      --tx-in "$TXIN" \
      --tx-out "$TXOUT" \
      --out-file "$ROOT/tx.body" 

if [ ! -f "$ROOT/tx.body" ]; then
  exit 1
fi

echo "transaction build"


cardano-cli conway transaction sign \
      --tx-body-file "$ROOT/tx.body" \
      --signing-key-file "$ROOT/utxo-keys/utxo1/utxo.skey" \
      --testnet-magic 42 \
      --out-file "$ROOT/tx.signed"

if [ ! -f "$ROOT/tx.signed" ]; then
  exit 1
fi


cardano-cli conway transaction submit \
      --socket-path "$ROOT/node.socket" \
      --tx-file "$ROOT/tx.signed" \
      --testnet-magic 42



if [ ! -f "$ROOT/wallet.addr" ]; then
  echo "error in post run genesis spo"
  exit 1
fi

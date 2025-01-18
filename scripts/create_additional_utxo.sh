#!/bin/sh
# generate additional utxos for genesis wallet
ROOT=$1
PKEYHEX=$(jq '.cborHex' < "$ROOT/utxo-keys/utxo1/utxo.skey")

# duplicate skey with different type field for ctl
echo "{\"type\":\"PaymentSigningKeyShelley_ed25519\",\"description\":\"Payment_Signing_Key\",\"cborHex\":$PKEYHEX}" > "$ROOT/wallet.skey"


cardano-cli conway address build \
  --payment-verification-key-file "$ROOT/utxo-keys/utxo1/utxo.vkey" \
  --out-file "$ROOT/wallet.addr" \
  --testnet-magic 42 



cardano-cli conway query utxo \
      --socket-path "$ROOT/node.socket" \
      --testnet-magic 42 \
      --address "$(cat "$ROOT/wallet.addr")" \
      --out-file "$ROOT/utxos.json"



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



cardano-cli conway transaction sign \
      --tx-body-file "$ROOT/tx.body" \
      --signing-key-file "$ROOT/utxo-keys/utxo1/utxo.skey" \
      --testnet-magic 42 \
      --out-file "$ROOT/tx.signed"


cardano-cli conway transaction submit \
      --socket-path "$ROOT/node.socket" \
      --tx-file "$ROOT/tx.signed" \
      --testnet-magic 42

touch "$ROOT/added_utxo"

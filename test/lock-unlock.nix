inputs@{ pkgs, cardano, ... }:
pkgs.writeShellApplication {
  name = "lock-unlock";
  runtimeInputs = [ pkgs.git cardano ];
  text = ''
    #!/bin/sh
    DIR=$(git rev-parse --show-toplevel)
    CONFIG="$DIR/cardano-conf"
    export CARDANO_NODE_SOCKET_PATH="$CONFIG/sockets/node-relay-1-socket/node.socket" 
    ROOT="/tmp"
    cd "$ROOT"


    # User-defined variables
    PAYMENT_SKEY="$CONFIG/utxo-keys/utxo1.skey"

    echo '{"type": "PlutusScriptV2","description": "", "cborHex": "52510100003222253330044a229309b2b2b9a1"}' > "$ROOT/script.plutus"

    SCRIPT_FILE="$ROOT/script.plutus"


    PAYMENT_ADDR="$(cat "genesis.addr")"

    cardano-cli address build \
        --payment-script-file "$SCRIPT_FILE" \
        --testnet-magic 2 \
        --out-file "script.addr"


    SCRIPT_ADDR="$(cat "script.addr")"

     

    if [[ "$1" == "lock" ]]; then
      echo "lock"
      cardano-cli query utxo \
            --testnet-magic 2 \
            --address "$(cat genesis.addr)" \
            --out-file genesis-utxos.json
      jq < "genesis-utxos.json" 

      AMOUNT_TO_SEND=10000000  # Amount of lovelace to send
      TX_IN=$(jq "keys[1]" "/tmp/genesis-utxos.json" --raw-output)

      cardano-cli query protocol-parameters \
          --testnet-magic 2 \
          --out-file protocol.json

      # Building the transaction
      cardano-cli transaction build \
          --testnet-magic 2 \
          --tx-in "$TX_IN" \
          --tx-out "$SCRIPT_ADDR"+"$AMOUNT_TO_SEND" \
          --tx-out-datum-embed-value "{}" \
          --change-address "$PAYMENT_ADDR" \
          --out-file tx.raw \
          --witness-override 2

      # Signing the transaction
      cardano-cli transaction sign \
          --tx-body-file tx.raw \
          --signing-key-file "$PAYMENT_SKEY" \
          --testnet-magic 2 \
          --out-file tx.signed

      # Submitting the transaction
      cardano-cli transaction submit \
          --tx-file tx.signed \
          --testnet-magic 2 
    fi


    if [[ "$1" == "unlock" ]]; then

    cardano-cli query protocol-parameters \
        --testnet-magic 2 \
        --out-file protocol.json

    cardano-cli query utxo \
          --testnet-magic 2 \
          --address "$SCRIPT_ADDR" \
          --out-file script-utxos.json
    cat script-utxos.json

    cardano-cli query utxo \
          --testnet-magic 2 \
          --address "$PAYMENT_ADDR" \
          --out-file genesis-utxos.json

    cat genesis-utxos.json

    SCRIPT_TX_IN=$(jq "keys[0]" "/tmp/script-utxos.json" --raw-output)
    COLLATERAL_TX_IN=$(jq "keys[0]" "/tmp/genesis-utxos.json" --raw-output)
    AMOUNT_TO_REDEEM=2000000
    echo "$SCRIPT_TX_IN"
    echo "$COLLATERAL_TX_IN"

    cardano-cli transaction build \
        --testnet-magic 2 \
        --tx-in "$SCRIPT_TX_IN"  \
        --tx-in-collateral "$COLLATERAL_TX_IN" \
        --tx-in-script-file "$SCRIPT_FILE" \
        --tx-in-datum-value "{}" \
        --tx-in-redeemer-value "{}" \
        --tx-out "$PAYMENT_ADDR"+"$AMOUNT_TO_REDEEM" \
        --change-address "$PAYMENT_ADDR" \
        --witness-override 2 \
        --out-file tx.raw 

    # Signing the transaction
    cardano-cli transaction sign \
        --tx-body-file tx.raw \
        --signing-key-file "$PAYMENT_SKEY" \
        --testnet-magic 2 \
        --out-file tx.signed

    # Submitting the transaction
    cardano-cli transaction submit \
        --tx-file tx.signed \
        --testnet-magic 2 
    
    fi
  
  '';
}

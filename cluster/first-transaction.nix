inputs@{ pkgs, cardano, ... }:
pkgs.writeShellApplication {
  name = "first-transaction";
  runtimeInputs = [ pkgs.git cardano ];
  text = ''
    #!/bin/sh
    DIR=$(git rev-parse --show-toplevel)
    CONFIG="$DIR/cardano-conf"
    CARDANO_NODE_SOCKET_PATH="$CONFIG/sockets/node-relay-1-socket/node.socket" 

    cardano-cli address build \
      --payment-verification-key-file "$CONFIG/utxo-keys/utxo1.vkey" \
      --out-file "/tmp/genesis.addr" \
      --testnet-magic 2 


    cardano-cli query utxo \
          --socket-path "$CARDANO_NODE_SOCKET_PATH" \
          --testnet-magic 2 \
          --address "$(cat /tmp/genesis.addr)" \
          --out-file "/tmp/genesis-utxos.json"

    TXIN=$(jq "keys[0]" "/tmp/genesis-utxos.json" --raw-output)
    SEND_AMT=2000000
    TXOUT="$(cat /tmp/genesis.addr)+$SEND_AMT"

    cardano-cli transaction build \
          --socket-path "$CARDANO_NODE_SOCKET_PATH" \
          --testnet-magic 2 \
          --change-address "$(cat /tmp/genesis.addr)" \
          --tx-in "$TXIN" \
          --tx-out "$TXOUT" \
          --out-file "/tmp/tx.body" \
          --witness-override 2


    cardano-cli transaction sign \
          --tx-body-file "/tmp/tx.body" \
          --signing-key-file "$CONFIG/utxo-keys/utxo1.skey" \
          --testnet-magic 2 \
          --out-file "/tmp/tx.signed"


    cardano-cli transaction submit \
          --socket-path "$CARDANO_NODE_SOCKET_PATH" \
          --tx-file "/tmp/tx.signed" \
          --testnet-magic 2
  '';
}

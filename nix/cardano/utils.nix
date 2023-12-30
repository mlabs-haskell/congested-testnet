{ inputs, self, ... }:
{
  perSystem = { system, inputs', self', pkgs, ... }:
    let
      runtimeInputs = [
        inputs'.cardano-node.legacyPackages.cardano-cli
        pkgs.jq
        pkgs.coreutils
        pkgs.websocat
        pkgs.curl
      ];
      gen-wallet = pkgs.writeShellApplication {
        name = "gen-wallet";
        inherit runtimeInputs;
        text = ''
          ROOT=$1

          # if [ -n "$(ls "$ROOT")" ]; then
          #   echo "volume is not empty"
          #   exit 0
          # fi

          cardano-cli address key-gen \
            --verification-key-file "$ROOT/wallet.vkey" \
            --signing-key-file "$ROOT/wallet.skey" 
          cat "$ROOT/wallet.skey"

          OGMIOS_REQUEST='{"params":{},"method":"queryLedgerState/utxo","jsonrpc":"2.0","id":"queryLedgerState/protocolParameters-5pyr568mlp9m1h8a"}'
          echo "$OGMIOS_REQUEST" | tr -d "\n" | websocat "ws://ogmios.local:1337"  | jq

          PUBKEYHEX=$( jq '.cborHex' < "$ROOT/wallet.vkey" ) 
          echo "$PUBKEYHEX"
          echo "{\"pubKeyHex\": $PUBKEYHEX}"

          curl -X POST "faucet.local:8000" -H "Content-Type: application/json" -d "{\"pubKeyHex\": $PUBKEYHEX }"

          ${self.packages.generate-scripts "$ROOT" "$PUBKEYHEX"}
          



          echo "=============================================================================="
        '';
      };
    in
    {
      packages.gen-wallet = gen-wallet;

      packages.make-faucet-wallet = pkgs.writeShellApplication {
        name = "make-faucet-wallet";
        inherit runtimeInputs;
        text = ''
          ROOT=$1
          CARDANO_NODE_SOCKET_PATH=$2
          CONFIG=$3

          if [ -n "$(ls "$ROOT")" ]; then
            echo "volume is not empty"
            exit 0
          fi

          while [ -z "$(ls -A "$CARDANO_NODE_SOCKET_PATH")" ]; do
            echo "socket is not ready"
            sleep 1
          done 

          PKEYHEX=$(jq '.cborHex' < "$CONFIG/utxo-keys/utxo1.skey")

          echo "{\"type\":\"PaymentSigningKeyShelley_ed25519\",\"description\":\"Payment_Signing_Key\",\"cborHex\":$PKEYHEX}" > "$ROOT/wallet.skey"

          cardano-cli address build \
            --payment-verification-key-file "$CONFIG/utxo-keys/utxo1.vkey" \
            --out-file "$ROOT/wallet.addr" \
            --testnet-magic 2 



          cardano-cli query utxo \
                --socket-path "$CARDANO_NODE_SOCKET_PATH/node.socket" \
                --testnet-magic 2 \
                --address "$(cat "$ROOT/wallet.addr")" \
                --out-file "$ROOT/utxos.json"

    
          TXIN=$(jq "keys[0]" "$ROOT/utxos.json" --raw-output)
          SEND_AMT=3000000
          TXOUT="$(cat "$ROOT/wallet.addr")+$SEND_AMT"

          cardano-cli transaction build \
                --socket-path "$CARDANO_NODE_SOCKET_PATH/node.socket" \
                --testnet-magic 2 \
                --change-address "$(cat "$ROOT/wallet.addr")" \
                --tx-in "$TXIN" \
                --tx-out "$TXOUT" \
                --out-file "$ROOT/tx.body" \
                --witness-override 2

    
          cardano-cli transaction sign \
                --tx-body-file "$ROOT/tx.body" \
                --signing-key-file "$CONFIG/utxo-keys/utxo1.skey" \
                --testnet-magic 2 \
                --out-file "$ROOT/tx.signed"
   
   
          cardano-cli transaction submit \
                --socket-path "$CARDANO_NODE_SOCKET_PATH/node.socket" \
                --tx-file "$ROOT/tx.signed" \
                --testnet-magic 2

        '';
      };

    };
}

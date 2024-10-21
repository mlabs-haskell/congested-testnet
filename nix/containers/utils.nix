{ inputs, self, ... }:
{
  perSystem = { system, inputs', self', pkgs, ... }:
    let
      runtimeInputs = [
        # self'.packages.cardano-node
        pkgs.cardano-node
        pkgs.jq
        pkgs.coreutils
        pkgs.gnugrep
        pkgs.websocat
        pkgs.curl
      ];
    in
    {

      packages.gen-wallet = pkgs.writeShellApplication {
        name = "gen-wallet";
        inherit runtimeInputs;
        text = ''
          set -e
          ROOT=$1
          url=$2

          ln -sf ${pkgs.iana-etc}/etc/protocols /etc/protocols
          ln -sf ${pkgs.iana-etc}/etc/services /etc/services
          echo "create wallet"

          if [ -f "$ROOT/wallet_exist" ]; then
            echo "wallet exist"
            exit 0
          fi

          cardano-cli address key-gen \
            --verification-key-file "$ROOT/wallet.vkey" \
            --signing-key-file "$ROOT/wallet.skey" 

          PUBKEYHASHHEX=$(cardano-cli address key-hash --payment-verification-key-file "$ROOT/wallet.vkey")
          echo "$PUBKEYHASHHEX"
          while true; do
              response=$(curl -X POST "$url" -H "Content-Type: application/json" -d "{\"pubKeyHashHex\": \"$PUBKEYHASHHEX\"}") 
              if echo "$response" | grep -q "Right"; then
                  echo "Success: got funds"
                  break
              else
                  echo "Fail: no funds"
                  sleep 1
              fi
          done

          ${self'.packages.generate-scripts}/bin/generate-scripts "$ROOT" "$PUBKEYHASHHEX"
          
          touch "$ROOT/wallet_exist" 
          echo "=============================================================================="
        '';
      };

      packages.make-faucet-wallet = pkgs.writeShellApplication {
        name = "make-faucet-wallet";
        inherit runtimeInputs;
        text = ''
          ROOT=$1
          CARDANO_NODE_SOCKET_PATH=$2
          CONFIG=$3

          if [ -f "$ROOT/faucet_wallet_exist" ]; then
            echo "faucet wallet exist"
            exit 0
          fi

          while [ ! -f "$CONFIG/finish_config" ]; do
            sleep 1
            echo 'Waiting for testnet configuration to be generated...'
          done


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

          touch "$ROOT/faucet_wallet_exist" 

        '';
      };

    };
}

{ inputs, self, ... }:
{
  perSystem = { system, inputs', self', pkgs, ... }:
    let
      runtimeInputs = [
        pkgs.cardano-node
        pkgs.cardano-cli
        pkgs.jq
        pkgs.coreutils
        pkgs.gnugrep
        pkgs.websocat
        pkgs.curl
      ];
    in
    {

      packages.generate-additional-utxo-for-ctl = pkgs.writeShellApplication {
        name = "generate-additional-utxo-for-ctl";
        inherit runtimeInputs;
        text = ''
          ROOT=$1
          CARDANO_NODE_SOCKET_PATH=$2
          CONFIG=$3

          while [ ! -f "$CONFIG/finish_config" ]; do
            sleep 1
            echo 'Waiting for testnet configuration to be generated...'
          done


          while [ -z "$(ls -A "$CARDANO_NODE_SOCKET_PATH")" ]; do
            echo "socket is not ready"
            sleep 1
          done 

          PKEYHEX=$(jq '.cborHex' < "$CONFIG/utxo-keys/utxo1/utxo.skey")

          echo "{\"type\":\"PaymentSigningKeyShelley_ed25519\",\"description\":\"Payment_Signing_Key\",\"cborHex\":$PKEYHEX}" > "$ROOT/wallet.skey"

          echo "build address"

          cardano-cli conway address build \
            --payment-verification-key-file "$CONFIG/utxo-keys/utxo1/utxo.vkey" \
            --out-file "$ROOT/wallet.addr" \
            --testnet-magic 42 

          echo "build address"


          cardano-cli conway query utxo \
                --socket-path "$CARDANO_NODE_SOCKET_PATH/node.socket" \
                --testnet-magic 42 \
                --address "$(cat "$ROOT/wallet.addr")" \
                --out-file "$ROOT/utxos.json"

          echo "query utxo"

    
          TXIN=$(jq "keys[0]" "$ROOT/utxos.json" --raw-output)
          SEND_AMT=3000000
          TXOUT="$(cat "$ROOT/wallet.addr")+$SEND_AMT"

          cardano-cli conway transaction build \
                --socket-path "$CARDANO_NODE_SOCKET_PATH/node.socket" \
                --testnet-magic 42 \
                --change-address "$(cat "$ROOT/wallet.addr")" \
                --tx-in "$TXIN" \
                --tx-out "$TXOUT" \
                --out-file "$ROOT/tx.body" 

          echo "transaction build"

    
          cardano-cli conway transaction sign \
                --tx-body-file "$ROOT/tx.body" \
                --signing-key-file "$CONFIG/utxo-keys/utxo1/utxo.skey" \
                --testnet-magic 42 \
                --out-file "$ROOT/tx.signed"


          cardano-cli conway transaction submit \
                --socket-path "$CARDANO_NODE_SOCKET_PATH/node.socket" \
                --tx-file "$ROOT/tx.signed" \
                --testnet-magic 42
          touch "$ROOT/faucet_wallet_exist" 

        '';
      };

    };
}

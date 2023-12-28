{ inputs, self, ... }:
{
  perSystem = { system, inputs', pkgs, ... }:
    {
      packages.make-faucet-wallet = pkgs.writeShellApplication {
        name = "make-faucet-wallet";
        runtimeInputs = [
          inputs'.cardano-node.legacyPackages.cardano-cli
          pkgs.jq
          pkgs.coreutils
        ];
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

          cardano-cli address build \
            --payment-verification-key-file "$CONFIG/utxo-keys/utxo1.vkey" \
            --out-file "$ROOT/wallet.addr" \
            --testnet-magic 2 

          cat "$ROOT/wallet.addr"
          echo  "$CARDANO_NODE_SOCKET_PATH"


          cardano-cli query utxo \
                --socket-path "$CARDANO_NODE_SOCKET_PATH/node.socket" \
                --testnet-magic 2 \
                --address "$(cat "$ROOT/wallet.addr")" \
                --out-file "$ROOT/utxos.json"
          cat "$ROOT/utxos.json"
          echo 123123123123
        '';
      };

    };
}

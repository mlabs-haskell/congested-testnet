{ pkgs, tags, cardano }:
let
  inherit (tags) cardano-tag;
in
pkgs.writeScriptBin "runnet" ''
  export CARDANO_CLI=${cardano}
  export ROOT=$(git rev-parse --show-toplevel)
  export CARDANO_TAG=${cardano-tag}
  ${./runnet.sh}
  while true; do
   if ${./ada-transfer-from-genesis-to-wallet0.sh} | grep -q "Transaction successfully submitted"; then
        break
    else
        echo "Transaction not successful yet. Retrying..."
        sleep 1
    fi
  done
''
